#define R_NO_REMAP
#include <R.h>
#include <Rinternals.h>
#include <R_ext/Utils.h>
#include <R_ext/Visibility.h>
#include <R_ext/GraphicsEngine.h>
#include <R_ext/GraphicsDevice.h>
#include <signal.h>
#ifdef _WIN32
#include <windows.h>
#else
#include <pthread.h>
#endif

/* Borrow the executable's state only for a protected, synchronous R dispatch. */
typedef struct {
    const char *config;
    const char *event;
    const char *instance;
    void *state;
    int result;
} plugin_invocation;

/* Allocation, lookup and evaluation all remain inside R's top-level error boundary. */
static void plugin_dispatch(void *data)
{
    plugin_invocation *invocation = data;
    SEXP package = PROTECT(Rf_mkString("eplusr"));
    SEXP ns = PROTECT(R_FindNamespace(package));
    SEXP config = PROTECT(Rf_mkString(invocation->config));
    SEXP event = PROTECT(Rf_mkString(invocation->event));
    SEXP instance = PROTECT(Rf_mkString(invocation->instance));
    SEXP state = PROTECT(R_MakeExternalPtr(invocation->state, R_NilValue, R_NilValue));
    SEXP fun = Rf_findFun(Rf_install("plugin_cli__dispatch"), ns);
    SEXP call = PROTECT(Rf_lang5(fun, config, event, instance, state));
    int error = 0;
    SEXP result = R_tryEval(call, ns, &error);
    invocation->result = error ? 1 : Rf_asInteger(result);
    UNPROTECT(7);
}

/* During R execution only, translate SIGINT into R's deferred interrupt flag. */
static void plugin_interrupt(int signal_number)
{
    (void)signal_number;
    R_interrupts_pending = 1;
}

/* Reject other threads before touching R; the Python adapter also checks its owner. */
#ifdef _WIN32
static DWORD plugin_owner;
#else
static pthread_t plugin_owner;
#endif
static int plugin_has_owner = 0;

/* Guard both bootstrap allocations and hook evaluation with the same native boundary. */
static int plugin_boundary(void (*fun)(void *), void *data)
{
#ifdef _WIN32
    DWORD current = GetCurrentThreadId();
    if (plugin_has_owner && current != plugin_owner) return 1;
#else
    pthread_t current = pthread_self();
    if (plugin_has_owner && !pthread_equal(current, plugin_owner)) return 1;
#endif
    plugin_owner = current;
    plugin_has_owner = 1;
#ifdef _WIN32
    void (*previous)(int) = signal(SIGINT, plugin_interrupt);
    if (previous == SIG_ERR) return 1;
#else
    struct sigaction handler = {0}, previous;
    handler.sa_handler = plugin_interrupt;
    sigemptyset(&handler.sa_mask);
    if (sigaction(SIGINT, &handler, &previous) != 0) return 1;
#endif
    int success = R_ToplevelExec(fun, data);
#ifdef _WIN32
    signal(SIGINT, previous);
#else
    sigaction(SIGINT, &previous, NULL);
#endif
    return success ? 0 : 1;
}

/* Load the R namespace without exposing allocation/parsing calls through ctypes. */
static void plugin_bootstrap(void *data)
{
    int *result = data;
    SEXP package = PROTECT(Rf_mkString("eplusr"));
    SEXP call = PROTECT(Rf_lang2(Rf_install("library"), package));
    R_tryEval(call, R_GlobalEnv, result);
    UNPROTECT(2);
}

/* The Python loader invokes bootstrap after initializing libR on its owner thread. */
int attribute_visible eplusr_plugin_bootstrap(void)
{
    int result = 1;
    if (plugin_boundary(plugin_bootstrap, &result)) return 1;
    return result;
}

/* Python calls this symbol synchronously; no R error crosses into the executable. */
int attribute_visible eplusr_plugin_dispatch(const char *config, const char *event,
                                            const char *instance, void *state)
{
    plugin_invocation invocation = {config, event, instance, state, 1};
    if (plugin_boundary(plugin_dispatch, &invocation)) return 1;
    return invocation.result;
}
