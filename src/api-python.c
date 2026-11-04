#ifndef _GNU_SOURCE
#define _GNU_SOURCE
#endif
#define R_NO_REMAP
#include <R.h>
#include <Rinternals.h>
#include <string.h>
#include <wchar.h>
#ifdef __linux__
#include <dlfcn.h>
#include <limits.h>
#include <stdlib.h>
#endif

/* Return copied dependency identity and a capability failure without exposing native addresses. */
static SEXP python_result(const char *version, const char *library, const char *error)
{
    SEXP result = PROTECT(Rf_allocVector(VECSXP, 4));
    SEXP names = PROTECT(Rf_allocVector(STRSXP, 4));
    const char *labels[] = {"version", "library", "wchar_bytes", "error"};
    for (int i = 0; i < 4; i++) SET_STRING_ELT(names, i, Rf_mkChar(labels[i]));
    SET_VECTOR_ELT(result, 0, version ? Rf_mkString(version) : Rf_ScalarString(NA_STRING));
    SET_VECTOR_ELT(result, 1, library ? Rf_mkString(library) : Rf_ScalarString(NA_STRING));
    SET_VECTOR_ELT(result, 2, Rf_ScalarInteger((int)sizeof(wchar_t)));
    SET_VECTOR_ELT(result, 3, error ? Rf_mkString(error) : R_NilValue);
    Rf_setAttrib(result, R_NamesSymbol, names);
    UNPROTECT(2);
    return result;
}

/* Repair only the reviewed Linux 9.6 CPython 3.7 pre-initialization boundary. */
SEXP eplusr_python_prepare(SEXP library, SEXP directory)
{
    if (TYPEOF(library) != EXTPTRSXP || R_ExternalPtrAddr(library) == NULL ||
        TYPEOF(directory) != STRSXP || XLENGTH(directory) != 1 || STRING_ELT(directory, 0) == NA_STRING)
        Rf_error("Expected the selected live library and installation directory");
#ifdef __linux__
    /* These signatures are from CPython 3.7's public pylifecycle.h, not private engine fields. */
    void *handle = R_ExternalPtrAddr(library);
    const char *symbols[] = {"Py_GetVersion", "Py_IsInitialized", "Py_SetProgramName"};
    void *addresses[3];
    Dl_info owners[3];
    for (int i = 0; i < 3; i++) {
        addresses[i] = dlsym(handle, symbols[i]);
        if (!addresses[i] || !dladdr(addresses[i], &owners[i]))
            return python_result(NULL, NULL, "Cannot identify the selected CPython public dependency");
    }
    if (owners[0].dli_fbase != owners[1].dli_fbase || owners[0].dli_fbase != owners[2].dli_fbase)
        return python_result(NULL, NULL, "Selected CPython public symbols do not belong to one library");
    const char *(*get_version)(void) = (const char *(*)(void))addresses[0];
    int (*is_initialized)(void) = (int (*)(void))addresses[1];
    void (*set_program_name)(const wchar_t *) = (void (*)(const wchar_t *))addresses[2];
    const char *version = get_version();
    char owner[PATH_MAX], root[PATH_MAX];
    if (!realpath(owners[0].dli_fname, owner) || !realpath(CHAR(STRING_ELT(directory, 0)), root))
        return python_result(version, owners[0].dli_fname, "Cannot resolve the selected CPython dependency location");
    size_t root_length = strlen(root);
    if (strncmp(owner, root, root_length) || owner[root_length] != '/')
        return python_result(version, owner, "The CPython dependency is outside the selected stock installation");
    if (!version || strncmp(version, "3.7.", 4) || sizeof(wchar_t) != 4)
        return python_result(version, owner, "The Linux 9.6 adapter requires the reviewed CPython 3.7 wide-character ABI");
    /* Reject global interposition without calling or reconfiguring a different Python runtime. */
    for (int i = 0; i < 3; i++) {
        void *global = dlsym(RTLD_DEFAULT, symbols[i]);
        if (global && global != addresses[i])
            return python_result(version, owner, "A different global CPython dependency would interpose the selected runtime");
    }
    if (is_initialized())
        return python_result(version, owner, "The selected CPython runtime is already initialized; retire its owning state or use a fresh worker");
    /* Stationary native wide storage outlives every synchronous call into this package. */
    static const wchar_t program_name[] = L"python";
    set_program_name(program_name);
    return python_result(version, owner, NULL);
#else
    return python_result(NULL, NULL, "This CPython adapter is only reviewed for Linux");
#endif
}
