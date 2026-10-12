#define R_NO_REMAP
#include <R.h>
#include <Rinternals.h>
#include <R_ext/Rdynload.h>
#include <R_ext/Visibility.h>
#include <stdatomic.h>
#include <string.h>
#ifdef _WIN32
#include <windows.h>
typedef DWORD guard_thread;
#define CURRENT_THREAD() GetCurrentThreadId()
#define SAME_THREAD(a,b) ((a)==(b))
#else
#include <pthread.h>
typedef pthread_t guard_thread;
#define CURRENT_THREAD() pthread_self()
#define SAME_THREAD(a,b) pthread_equal((a),(b))
#endif

/* EnergyPlus has four void-return callback shapes. Slots keep R out of foreign threads. */
typedef struct {
    void *original;
    void *proxy;
    guard_thread owner;
    atomic_uint foreign_calls;
} callback_guard;
static callback_guard guards[256];

/* A foreign call may only update native atomic state, never allocate or call the R API. */
static int guard_enter(int index)
{
    callback_guard *guard = &guards[index];
    if (!guard->original) return 0;
    if (!SAME_THREAD(guard->owner, CURRENT_THREAD())) {
        atomic_fetch_add_explicit(&guard->foreign_calls, 1, memory_order_relaxed);
        return 0;
    }
    return atomic_load_explicit(&guard->foreign_calls, memory_order_relaxed) == 0;
}

/* Expand per-slot entry points: native APIs provide no user-data parameter for callbacks. */
#define DECLARE_PROXY(N) \
static void state_##N(void *s) { if (guard_enter(N)) ((void (*)(void *))guards[N].original)(s); } \
static void progress_##N(int i) { if (guard_enter(N)) ((void (*)(int))guards[N].original)(i); } \
static void message_##N(const char *s) { if (guard_enter(N)) ((void (*)(const char *))guards[N].original)(s); } \
static void error_##N(int i, const char *s) { if (guard_enter(N)) ((void (*)(int,const char *))guards[N].original)(i,s); }
#define SLOTS(X) X(0) X(1) X(2) X(3) X(4) X(5) X(6) X(7) \
    X(8) X(9) X(10) X(11) X(12) X(13) X(14) X(15) \
    X(16) X(17) X(18) X(19) X(20) X(21) X(22) X(23) \
    X(24) X(25) X(26) X(27) X(28) X(29) X(30) X(31) \
    X(32) X(33) X(34) X(35) X(36) X(37) X(38) X(39) \
    X(40) X(41) X(42) X(43) X(44) X(45) X(46) X(47) \
    X(48) X(49) X(50) X(51) X(52) X(53) X(54) X(55) \
    X(56) X(57) X(58) X(59) X(60) X(61) X(62) X(63) \
    X(64) X(65) X(66) X(67) X(68) X(69) X(70) X(71) \
    X(72) X(73) X(74) X(75) X(76) X(77) X(78) X(79) \
    X(80) X(81) X(82) X(83) X(84) X(85) X(86) X(87) \
    X(88) X(89) X(90) X(91) X(92) X(93) X(94) X(95) \
    X(96) X(97) X(98) X(99) X(100) X(101) X(102) X(103) \
    X(104) X(105) X(106) X(107) X(108) X(109) X(110) X(111) \
    X(112) X(113) X(114) X(115) X(116) X(117) X(118) X(119) \
    X(120) X(121) X(122) X(123) X(124) X(125) X(126) X(127) \
    X(128) X(129) X(130) X(131) X(132) X(133) X(134) X(135) \
    X(136) X(137) X(138) X(139) X(140) X(141) X(142) X(143) \
    X(144) X(145) X(146) X(147) X(148) X(149) X(150) X(151) \
    X(152) X(153) X(154) X(155) X(156) X(157) X(158) X(159) \
    X(160) X(161) X(162) X(163) X(164) X(165) X(166) X(167) \
    X(168) X(169) X(170) X(171) X(172) X(173) X(174) X(175) \
    X(176) X(177) X(178) X(179) X(180) X(181) X(182) X(183) \
    X(184) X(185) X(186) X(187) X(188) X(189) X(190) X(191) \
    X(192) X(193) X(194) X(195) X(196) X(197) X(198) X(199) \
    X(200) X(201) X(202) X(203) X(204) X(205) X(206) X(207) \
    X(208) X(209) X(210) X(211) X(212) X(213) X(214) X(215) \
    X(216) X(217) X(218) X(219) X(220) X(221) X(222) X(223) \
    X(224) X(225) X(226) X(227) X(228) X(229) X(230) X(231) \
    X(232) X(233) X(234) X(235) X(236) X(237) X(238) X(239) \
    X(240) X(241) X(242) X(243) X(244) X(245) X(246) X(247) \
    X(248) X(249) X(250) X(251) X(252) X(253) X(254) X(255)
SLOTS(DECLARE_PROXY)
#define STATE_PROXY(N) (void *)state_##N,
#define PROGRESS_PROXY(N) (void *)progress_##N,
#define MESSAGE_PROXY(N) (void *)message_##N,
#define ERROR_PROXY(N) (void *)error_##N,
static void *proxies[4][256] = {
    {SLOTS(STATE_PROXY)}, {SLOTS(PROGRESS_PROXY)}, {SLOTS(MESSAGE_PROXY)}, {SLOTS(ERROR_PROXY)}
};

/* The owning R thread validates a guard before reading its protected original callback. */
static callback_guard *guard_data(SEXP object)
{
    if (TYPEOF(object) != EXTPTRSXP || R_ExternalPtrAddr(object) == NULL)
        Rf_error("Callback guard is closed or was serialized");
    SEXP tag = R_ExternalPtrTag(object);
    if (TYPEOF(tag) != INTSXP || XLENGTH(tag) != 1 || INTEGER(tag)[0] < 0 || INTEGER(tag)[0] >= 256)
        Rf_error("Invalid callback guard");
    callback_guard *guard = &guards[INTEGER(tag)[0]];
    if (guard->proxy != R_ExternalPtrAddr(object) || guard->original == NULL)
        Rf_error("Callback guard is no longer registered");
    return guard;
}

/* Release only after the engine reset/deleted the state and ended its native registrations. */
static void guard_finalizer(SEXP object)
{
    if (R_ExternalPtrAddr(object) == NULL) return;
    callback_guard *guard = guard_data(object);
    guard->original = NULL;
    guard->proxy = NULL;
    R_ClearExternalPtr(object);
    R_SetExternalPtrProtected(object, R_NilValue);
}

/* Protect the rdyncall pointer as part of the proxy so GC cannot remove the real trampoline. */
static SEXP guard_create(SEXP original, SEXP signature)
{
    if (TYPEOF(original) != EXTPTRSXP || R_ExternalPtrAddr(original) == NULL)
        Rf_error("Expected a live rdyncall callback pointer");
    const char *sig = CHAR(Rf_asChar(signature));
    int kind = strcmp(sig, "p)v") == 0 ? 0 : strcmp(sig, "i)v") == 0 ? 1 :
               strcmp(sig, "Z)v") == 0 ? 2 : strcmp(sig, "iZ)v") == 0 ? 3 : -1;
    if (kind < 0) Rf_error("Unsupported EnergyPlus callback signature");
    int index;
    for (index = 0; index < 256; index++) if (!guards[index].original) break;
    if (index == 256) Rf_error("At most 256 EnergyPlus callbacks may be registered in one R process");
    callback_guard *guard = &guards[index];
    guard->owner = CURRENT_THREAD();
    atomic_store_explicit(&guard->foreign_calls, 0, memory_order_relaxed);
    guard->original = R_ExternalPtrAddr(original);
    guard->proxy = proxies[kind][index];
    SEXP tag = PROTECT(Rf_ScalarInteger(index));
    SEXP output = PROTECT(R_MakeExternalPtr(guard->proxy, tag, original));
    R_RegisterCFinalizerEx(output, guard_finalizer, TRUE);
    UNPROTECT(2);
    return output;
}

/* Read a native counter on the owning R thread; conversion happens after the foreign call. */
static SEXP guard_foreign(SEXP object)
{
    return Rf_ScalarReal(atomic_load_explicit(&guard_data(object)->foreign_calls, memory_order_relaxed));
}

/* Return the pinned original for rdyncall's diagnostic API, without exposing it publicly. */
static SEXP guard_original(SEXP object)
{
    (void)guard_data(object);
    return R_ExternalPtrProtected(object);
}

/* Explicit retirement permits slot reuse without waiting for R's next collection. */
static SEXP guard_release(SEXP object)
{
    guard_finalizer(object);
    return R_NilValue;
}

/* Register only the private native seam; no EnergyPlus or rdyncall headers are needed here. */
static const R_CallMethodDef call_entries[] = {
    {"eplusr_guard_create", (DL_FUNC)&guard_create, 2},
    {"eplusr_guard_foreign", (DL_FUNC)&guard_foreign, 1},
    {"eplusr_guard_original", (DL_FUNC)&guard_original, 1},
    {"eplusr_guard_release", (DL_FUNC)&guard_release, 1},
    {NULL, NULL, 0}
};

/* Package initialization only registers C routines; it does not load the optional engine. */
void attribute_visible R_init_eplusr(DllInfo *dll)
{
    R_registerRoutines(dll, NULL, call_entries, NULL, NULL);
    R_useDynamicSymbols(dll, FALSE);
}
