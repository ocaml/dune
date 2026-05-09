#define CAML_INTERNALS

#include <caml/alloc.h>
#include <caml/custom.h>
#include <caml/fail.h>
#include <caml/memory.h>
#include <caml/mlvalues.h>
#include <caml/signals.h>
#include <caml/threads.h>
#include <caml/unixsupport.h>

#include <errno.h>
#include <signal.h>

#ifdef _WIN32

#define WIN32_LEAN_AND_MEAN
#include <windows.h>

#else

#include <pthread.h>
#include <semaphore.h>
#include <stdlib.h>
#include <unistd.h>

#if defined(__APPLE__) && defined(__MACH__)
#include <fcntl.h>
#include <stdio.h>
#define DUNE_USE_NAMED_SEMAPHORE 1
#else
#define DUNE_USE_NAMED_SEMAPHORE 0
#endif

#endif

#ifndef NSIG
#define NSIG 128
#endif

#define DUNE_MAX_WATCHED_SIGNALS 32
/* Keep enough repeated deliveries to preserve the emergency Ctrl+C path, while
   bounding the amount of scheduler work caused by a signal storm. The Unix
   lock-free atomic implementation stores these as byte counters. */
#define DUNE_MAX_PENDING_PER_SIGNAL 3

#ifdef _WIN32
typedef LONG signal_counter;
static volatile signal_counter dune_pending_signals[NSIG];
static volatile LONG signal_wakeup_pending = 0;

static void pending_signal_clear(int signo)
{
  InterlockedExchange(&dune_pending_signals[signo], 0);
}

static void pending_signal_increment(int signo)
{
  LONG count = InterlockedIncrement(&dune_pending_signals[signo]);
  if (count > DUNE_MAX_PENDING_PER_SIGNAL)
    InterlockedExchange(&dune_pending_signals[signo], DUNE_MAX_PENDING_PER_SIGNAL);
}

static int pending_signal_take_one(int signo)
{
  LONG count;

  do {
    count = InterlockedCompareExchange(&dune_pending_signals[signo], 0, 0);
    if (count <= 0) return 0;
  } while (InterlockedCompareExchange(&dune_pending_signals[signo], count - 1,
                                      count)
           != count);

  return 1;
}

static int pending_signal_load(int signo)
{
  return (int)InterlockedCompareExchange(&dune_pending_signals[signo], 0, 0);
}

static int signal_wakeup_mark_pending(void)
{
  return InterlockedCompareExchange(&signal_wakeup_pending, 1, 0) == 0;
}

static void signal_wakeup_clear_pending(void)
{
  InterlockedExchange(&signal_wakeup_pending, 0);
}
#elif defined(__GNUC__) && defined(__GCC_ATOMIC_CHAR_LOCK_FREE) &&              \
    __GCC_ATOMIC_CHAR_LOCK_FREE == 2
typedef unsigned char signal_counter;
static signal_counter dune_pending_signals[NSIG];
static signal_counter signal_wakeup_pending = 0;

static void pending_signal_clear(int signo)
{
  __atomic_store_n(&dune_pending_signals[signo], 0, __ATOMIC_RELAXED);
}

static void pending_signal_increment(int signo)
{
  signal_counter count =
      __atomic_load_n(&dune_pending_signals[signo], __ATOMIC_RELAXED);

  while (count < DUNE_MAX_PENDING_PER_SIGNAL) {
    signal_counter next = count + 1;
    if (__atomic_compare_exchange_n(&dune_pending_signals[signo], &count, next,
                                    0, __ATOMIC_RELAXED, __ATOMIC_RELAXED))
      return;
  }
}

static int pending_signal_take_one(int signo)
{
  signal_counter count =
      __atomic_load_n(&dune_pending_signals[signo], __ATOMIC_RELAXED);

  while (count > 0) {
    signal_counter next = count - 1;
    if (__atomic_compare_exchange_n(&dune_pending_signals[signo], &count, next,
                                    0, __ATOMIC_RELAXED, __ATOMIC_RELAXED))
      return 1;
  }

  return 0;
}

static int pending_signal_load(int signo)
{
  return (int)__atomic_load_n(&dune_pending_signals[signo], __ATOMIC_RELAXED);
}

static int signal_wakeup_mark_pending(void)
{
  signal_counter expected = 0;
  return __atomic_compare_exchange_n(&signal_wakeup_pending, &expected, 1, 0,
                                     __ATOMIC_RELAXED, __ATOMIC_RELAXED);
}

static void signal_wakeup_clear_pending(void)
{
  __atomic_store_n(&signal_wakeup_pending, 0, __ATOMIC_RELAXED);
}
#else
typedef sig_atomic_t signal_counter;
static volatile signal_counter dune_pending_signals[NSIG];
static volatile sig_atomic_t signal_wakeup_pending = 0;

static void pending_signal_clear(int signo)
{
  dune_pending_signals[signo] = 0;
}

static void pending_signal_increment(int signo)
{
  if (dune_pending_signals[signo] < DUNE_MAX_PENDING_PER_SIGNAL)
    dune_pending_signals[signo]++;
}

static int pending_signal_take_one(int signo)
{
  if (dune_pending_signals[signo] <= 0) return 0;
  dune_pending_signals[signo]--;
  return 1;
}

static int pending_signal_load(int signo)
{
  return dune_pending_signals[signo];
}

static int signal_wakeup_mark_pending(void)
{
  if (signal_wakeup_pending) return 0;
  signal_wakeup_pending = 1;
  return 1;
}

static void signal_wakeup_clear_pending(void)
{
  signal_wakeup_pending = 0;
}
#endif

static int watched_count = 0;
static int watched_signals[DUNE_MAX_WATCHED_SIGNALS];
static int watched_ocaml_signals[DUNE_MAX_WATCHED_SIGNALS];
static value signal_wakeup_root = Val_unit;
static int signal_wakeup_root_registered = 0;

static void register_signal_wakeup_root(void)
{
  if (!signal_wakeup_root_registered) {
    caml_register_global_root(&signal_wakeup_root);
    signal_wakeup_root_registered = 1;
  }
}

static void dune_event_wakeup_finalize(value v_wakeup);

static struct custom_operations dune_event_wakeup_ops = {
  "build.dune.scheduler.event_wakeup",
  dune_event_wakeup_finalize,
  custom_compare_default,
  custom_hash_default,
  custom_serialize_default,
  custom_deserialize_default,
  custom_compare_ext_default,
  custom_fixed_length_default
};

#ifdef _WIN32

struct dune_event_wakeup {
  HANDLE event;
  int used_for_signals;
};

static PVOID volatile signal_wakeup = NULL;

#define Dune_event_wakeup_val(v) ((struct dune_event_wakeup *)Data_custom_val(v))

static void dune_event_wakeup_finalize(value v_wakeup)
{
  struct dune_event_wakeup *wakeup = Dune_event_wakeup_val(v_wakeup);
  if (wakeup->used_for_signals) return;
  if (wakeup->event != NULL) CloseHandle(wakeup->event);
}

static void dune_signal_wakeup(void)
{
  DWORD saved_error = GetLastError();
  HANDLE event =
      (HANDLE)InterlockedCompareExchangePointer(&signal_wakeup, NULL, NULL);
  if (event != NULL && signal_wakeup_mark_pending()) SetEvent(event);
  SetLastError(saved_error);
}

CAMLprim value dune_event_wakeup_create(value v_unit)
{
  CAMLparam1(v_unit);
  CAMLlocal1(v_wakeup);
  struct dune_event_wakeup *wakeup;

  v_wakeup = caml_alloc_custom(&dune_event_wakeup_ops,
                               sizeof(struct dune_event_wakeup), 0, 1);
  wakeup = Dune_event_wakeup_val(v_wakeup);
  wakeup->event = NULL;
  wakeup->used_for_signals = 0;
  wakeup->event = CreateEvent(NULL, FALSE, FALSE, NULL);
  if (wakeup->event == NULL) caml_failwith("CreateEvent");

  CAMLreturn(v_wakeup);
}

CAMLprim value dune_event_wakeup_release(value v_wakeup)
{
  CAMLparam1(v_wakeup);
  struct dune_event_wakeup *wakeup = Dune_event_wakeup_val(v_wakeup);
  if (!SetEvent(wakeup->event)) caml_failwith("SetEvent");
  CAMLreturn(Val_unit);
}

CAMLprim value dune_event_wakeup_acquire(value v_wakeup)
{
  CAMLparam1(v_wakeup);
  struct dune_event_wakeup *wakeup = Dune_event_wakeup_val(v_wakeup);
  HANDLE event = wakeup->event;
  DWORD result;

  caml_release_runtime_system();
  result = WaitForSingleObject(event, INFINITE);
  caml_acquire_runtime_system();

  if (result != WAIT_OBJECT_0) caml_failwith("WaitForSingleObject");
  signal_wakeup_clear_pending();
  CAMLreturn(Val_unit);
}

CAMLprim value dune_event_wakeup_try_acquire(value v_wakeup)
{
  CAMLparam1(v_wakeup);
  struct dune_event_wakeup *wakeup = Dune_event_wakeup_val(v_wakeup);
  DWORD result = WaitForSingleObject(wakeup->event, 0);
  if (result == WAIT_OBJECT_0) {
    signal_wakeup_clear_pending();
    CAMLreturn(Val_true);
  }
  if (result == WAIT_TIMEOUT) CAMLreturn(Val_false);
  caml_failwith("WaitForSingleObject");
}

CAMLprim value dune_event_wakeup_use_for_signals(value v_wakeup)
{
  CAMLparam1(v_wakeup);
  struct dune_event_wakeup *wakeup = Dune_event_wakeup_val(v_wakeup);
  register_signal_wakeup_root();
  signal_wakeup_root = v_wakeup;
  wakeup->used_for_signals = 1;
  InterlockedExchangePointer(&signal_wakeup, wakeup->event);
  /* A previous scheduler run may have left a stale wakeup token on its
     event. */
  signal_wakeup_clear_pending();
  CAMLreturn(Val_unit);
}

CAMLprim value dune_event_wakeup_stop_using_signals(value v_wakeup)
{
  CAMLparam1(v_wakeup);
  struct dune_event_wakeup *wakeup = Dune_event_wakeup_val(v_wakeup);
  PVOID previous =
      InterlockedCompareExchangePointer(&signal_wakeup, NULL, wakeup->event);
  if (previous == wakeup->event) {
    signal_wakeup_clear_pending();
    wakeup->used_for_signals = 0;
    signal_wakeup_root = Val_unit;
  }
  CAMLreturn(Val_unit);
}

#else

struct dune_event_wakeup {
  sem_t *sem;
  int used_for_signals;
};

static sem_t *volatile signal_wakeup = NULL;

#define Dune_event_wakeup_val(v) ((struct dune_event_wakeup *)Data_custom_val(v))

static void dune_event_wakeup_finalize(value v_wakeup)
{
  struct dune_event_wakeup *wakeup = Dune_event_wakeup_val(v_wakeup);
  if (wakeup->used_for_signals) return;
  if (wakeup->sem == NULL) return;
#if DUNE_USE_NAMED_SEMAPHORE
  sem_close(wakeup->sem);
#else
  sem_destroy(wakeup->sem);
  free(wakeup->sem);
#endif
}

static sem_t *create_semaphore(void)
{
#if DUNE_USE_NAMED_SEMAPHORE
  static int semaphore_id = 0;
  char name[64];
  sem_t *sem;

  for (int i = 0; i < 100; i++) {
    snprintf(name, sizeof(name), "/dune-%ld-%d", (long)getpid(), semaphore_id++);
    sem = sem_open(name, O_CREAT | O_EXCL, 0600, 0);
    if (sem != SEM_FAILED) {
      if (sem_unlink(name) == -1) {
        sem_close(sem);
        uerror("sem_unlink", Nothing);
      }
      return sem;
    }
    if (errno != EEXIST) uerror("sem_open", Nothing);
  }

  caml_failwith("sem_open");
#else
  sem_t *sem = malloc(sizeof(sem_t));
  if (sem == NULL) caml_raise_out_of_memory();
  if (sem_init(sem, 0, 0) == -1) {
    free(sem);
    uerror("sem_init", Nothing);
  }
  return sem;
#endif
}

static void dune_signal_wakeup(void)
{
  int saved_errno = errno;
  sem_t *sem = signal_wakeup;

  if (sem != NULL && signal_wakeup_mark_pending()) {
    int result = sem_post(sem);
    if (result == -1) signal_wakeup_clear_pending();
    (void)result;
  }

  errno = saved_errno;
}

static int wait_for_semaphore(sem_t *sem)
{
  int result;

  do {
    result = sem_wait(sem);
  } while (result == -1 && errno == EINTR);

  return result;
}

static int try_wait_for_semaphore(sem_t *sem)
{
  int result;

  do {
    result = sem_trywait(sem);
  } while (result == -1 && errno == EINTR);

  return result;
}

CAMLprim value dune_event_wakeup_create(value v_unit)
{
  CAMLparam1(v_unit);
  CAMLlocal1(v_wakeup);
  struct dune_event_wakeup *wakeup;

  v_wakeup = caml_alloc_custom(&dune_event_wakeup_ops,
                               sizeof(struct dune_event_wakeup), 0, 1);
  wakeup = Dune_event_wakeup_val(v_wakeup);
  wakeup->sem = NULL;
  wakeup->used_for_signals = 0;
  wakeup->sem = create_semaphore();

  CAMLreturn(v_wakeup);
}

CAMLprim value dune_event_wakeup_release(value v_wakeup)
{
  CAMLparam1(v_wakeup);
  struct dune_event_wakeup *wakeup = Dune_event_wakeup_val(v_wakeup);
  if (sem_post(wakeup->sem) == -1) uerror("sem_post", Nothing);
  CAMLreturn(Val_unit);
}

CAMLprim value dune_event_wakeup_acquire(value v_wakeup)
{
  CAMLparam1(v_wakeup);
  struct dune_event_wakeup *wakeup = Dune_event_wakeup_val(v_wakeup);
  sem_t *sem = wakeup->sem;
  int result;
  int saved_errno;

  caml_release_runtime_system();
  result = wait_for_semaphore(sem);
  saved_errno = errno;
  caml_acquire_runtime_system();

  if (result == -1) {
    errno = saved_errno;
    uerror("sem_wait", Nothing);
  }

  signal_wakeup_clear_pending();
  CAMLreturn(Val_unit);
}

CAMLprim value dune_event_wakeup_try_acquire(value v_wakeup)
{
  CAMLparam1(v_wakeup);
  struct dune_event_wakeup *wakeup = Dune_event_wakeup_val(v_wakeup);
  if (try_wait_for_semaphore(wakeup->sem) == 0) {
    signal_wakeup_clear_pending();
    CAMLreturn(Val_true);
  }
  if (errno == EAGAIN) CAMLreturn(Val_false);

  uerror("sem_trywait", Nothing);
}

CAMLprim value dune_event_wakeup_use_for_signals(value v_wakeup)
{
  CAMLparam1(v_wakeup);
  struct dune_event_wakeup *wakeup = Dune_event_wakeup_val(v_wakeup);
  register_signal_wakeup_root();
  signal_wakeup_root = v_wakeup;
  wakeup->used_for_signals = 1;
  signal_wakeup = wakeup->sem;
  /* A previous scheduler run may have left a stale wakeup token on its
     semaphore. */
  signal_wakeup_clear_pending();
  CAMLreturn(Val_unit);
}

CAMLprim value dune_event_wakeup_stop_using_signals(value v_wakeup)
{
  CAMLparam1(v_wakeup);
  struct dune_event_wakeup *wakeup = Dune_event_wakeup_val(v_wakeup);
  if (signal_wakeup == wakeup->sem) {
    signal_wakeup = NULL;
    signal_wakeup_clear_pending();
    wakeup->used_for_signals = 0;
    signal_wakeup_root = Val_unit;
  }
  CAMLreturn(Val_unit);
}

#endif

static void signal_handler(int signo)
{
#ifdef _WIN32
  signal(signo, signal_handler);
#endif
  if (signo >= 0 && signo < NSIG) pending_signal_increment(signo);
  dune_signal_wakeup();
}

CAMLprim value dune_signal_watcher_install(value v_signals)
{
  CAMLparam1(v_signals);

  for (int i = 0; i < NSIG; i++) pending_signal_clear(i);
  watched_count = 0;

  for (; v_signals != Val_emptylist; v_signals = Field(v_signals, 1)) {
    int ocaml_signal = Int_val(Field(v_signals, 0));
    int signo = caml_convert_signal_number(ocaml_signal);

    if (signo < 0 || signo >= NSIG) continue;
    if (watched_count == DUNE_MAX_WATCHED_SIGNALS)
      caml_invalid_argument("too many watched signals");

    watched_signals[watched_count] = signo;
    watched_ocaml_signals[watched_count] = ocaml_signal;
    watched_count++;

#ifdef _WIN32
    if (signal(signo, signal_handler) == SIG_ERR) caml_failwith("signal");
#else
    struct sigaction sa;
    sigemptyset(&sa.sa_mask);
    sa.sa_handler = signal_handler;
    sa.sa_flags = 0;
#ifdef SA_RESTART
    sa.sa_flags |= SA_RESTART;
#endif
    if (sigaction(signo, &sa, NULL) == -1) uerror("sigaction", Nothing);
#endif
  }

  CAMLreturn(Val_unit);
}

CAMLprim value dune_signal_watcher_next_pending(value v_unit)
{
  CAMLparam1(v_unit);
  int result = 0;

  (void)v_unit;

#ifndef _WIN32
  sigset_t set;
  sigset_t previous_set;

  sigemptyset(&set);
  for (int i = 0; i < watched_count; i++) sigaddset(&set, watched_signals[i]);
  if (pthread_sigmask(SIG_BLOCK, &set, &previous_set) != 0)
    caml_failwith("pthread_sigmask");
#endif

  for (int i = 0; i < watched_count; i++) {
    int signo = watched_signals[i];
    if (pending_signal_take_one(signo)) {
      result = watched_ocaml_signals[i];
      break;
    }
  }

#ifndef _WIN32
  if (pthread_sigmask(SIG_SETMASK, &previous_set, NULL) != 0)
    caml_failwith("pthread_sigmask");
#endif

  CAMLreturn(Val_int(result));
}

CAMLprim value dune_signal_watcher_has_pending(value v_unit)
{
  (void)v_unit;

  for (int i = 0; i < watched_count; i++) {
    if (pending_signal_load(watched_signals[i]) > 0) return Val_true;
  }

  return Val_false;
}
