#ifdef _WIN32
#include <windows.h>
#else
#include <pthread.h>
#endif

/* The fixture forwards four exact EnergyPlus callback shapes from either thread. */
typedef struct { void *callback; int kind; } callback_task;

/* Payloads have no R dependencies, so a foreign-thread failure cannot touch R. */
static void invoke(callback_task *task)
{
    switch (task->kind) {
    case 0: ((void (*)(void *))task->callback)(NULL); break;
    case 1: ((void (*)(int))task->callback)(67); break;
    case 2: ((void (*)(const char *))task->callback)("message"); break;
    case 3: ((void (*)(int,const char *))task->callback)(2, "warning"); break;
    }
}

/* This is the normal synchronous caller on the owning R thread. */
void callback_on_owner(void *callback, int kind)
{
    callback_task task = {callback, kind};
    invoke(&task);
}

/* Run and join the native worker before returning to R; it never invokes an R API. */
#ifdef _WIN32
static DWORD WINAPI worker(LPVOID task) { invoke((callback_task *)task); return 0; }
#else
static void *worker(void *task) { invoke((callback_task *)task); return NULL; }
#endif

/* Joining makes proxy retirement safe once this foreign invocation is complete. */
int callback_on_worker(void *callback, int kind)
{
    callback_task task = {callback, kind};
#ifdef _WIN32
    HANDLE thread = CreateThread(NULL, 0, worker, &task, 0, NULL);
    if (!thread) return 1;
    WaitForSingleObject(thread, INFINITE);
    CloseHandle(thread);
#else
    pthread_t thread;
    if (pthread_create(&thread, NULL, worker, &task)) return 1;
    pthread_join(thread, NULL);
#endif
    return 0;
}

#define R_NO_REMAP
#include <R.h>
#include <Rinternals.h>
#include <R_ext/Utils.h>
#include <signal.h>

/* Retain an engine-visible proxy address after its owning guard is retired. */
SEXP callback_retain(SEXP guard)
{
    return R_MakeExternalPtr(R_ExternalPtrAddr(guard), R_NilValue, guard);
}

/* Only native addresses and plain integer results cross the plugin worker boundary. */
typedef struct {
    int (*bootstrap)(void);
    int (*dispatch)(const char *, const char *, const char *, void *);
    int bootstrap_result;
    int dispatch_result;
} plugin_task;

/* The joined worker must reject both installed plugin entry points before touching R. */
#ifdef _WIN32
static DWORD WINAPI plugin_worker(LPVOID data)
#else
static void *plugin_worker(void *data)
#endif
{
    plugin_task *task = (plugin_task *)data;
    task->bootstrap_result = task->bootstrap();
    task->dispatch_result = task->dispatch("", "", "", NULL);
#ifdef _WIN32
    return 0;
#else
    return NULL;
#endif
}

/* Encode both statuses independently; creation failure is distinct from rejection. */
int plugin_on_worker(void *bootstrap, void *dispatch)
{
    plugin_task task = {bootstrap, dispatch, -1, -1};
#ifdef _WIN32
    HANDLE thread = CreateThread(NULL, 0, plugin_worker, &task, 0, NULL);
    if (!thread) return -1;
    WaitForSingleObject(thread, INFINITE);
    CloseHandle(thread);
#else
    pthread_t thread;
    if (pthread_create(&thread, NULL, plugin_worker, &task)) return -1;
    pthread_join(thread, NULL);
#endif
    return 10 * task.bootstrap_result + task.dispatch_result;
}

/* Compare the process SIGINT handler before and after each protected native entry. */
int plugin_signal_handler(int save)
{
#ifdef _WIN32
    static void (*saved)(int);
    void (*current)(int) = signal(SIGINT, SIG_IGN);
    signal(SIGINT, current);
#else
    static void (*saved)(int);
    struct sigaction current;
    if (sigaction(SIGINT, NULL, &current)) return -1;
#endif
#ifdef _WIN32
    if (save) saved = current;
    return saved == current;
#else
    if (save) saved = current.sa_handler;
    return saved == current.sa_handler;
#endif
}

/* Deliver an actual OS interrupt inside the installed plugin's R top-level boundary. */
void plugin_raise_interrupt(void)
{
    raise(SIGINT);
    R_CheckUserInterrupt();
}
