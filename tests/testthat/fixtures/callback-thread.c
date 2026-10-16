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
