#define _GNU_SOURCE

/* Must come before any other caml/ headers are included */
#define CAML_INTERNALS

#ifdef _WIN32
/* for [caml_win32_multi_byte_to_wide_char] */
#include <caml/osdeps.h>

/* Prior to OCaml 5.0, the function was called win_multi_byte_to_wide_char */
#include <caml/version.h>
#if OCAML_VERSION_MAJOR < 5
#define caml_win32_multi_byte_to_wide_char win_multi_byte_to_wide_char
#define caml_win32_maperr win32_maperr
#endif
#endif

#include <caml/mlvalues.h>
#include <caml/memory.h>
#include <caml/alloc.h>
#include <caml/unixsupport.h>
#include <caml/fail.h>

/* for [caml_convert_signal_number]; must come after public caml headers */
#include <caml/signals.h>

#include <errno.h>

#if defined(__APPLE__)

# if defined(__MAC_OS_X_VERSION_MAX_ALLOWED)
#  define USE_POSIX_SPAWN
#  define vfork fork
# endif

#include <fcntl.h>
#include <sys/socket.h>
#include <sys/syscall.h>
#include <sys/types.h>
#include <unistd.h>

#ifndef SYS___pthread_chdir
#define SYS___pthread_chdir 348
#endif
#ifndef SYS___pthread_fchdir
#define SYS___pthread_fchdir 349
#endif

static int __pthread_chdir(const char *path) {
#pragma clang diagnostic push
#pragma clang diagnostic ignored "-Wdeprecated"
  return syscall(SYS___pthread_chdir, path);
#pragma clang diagnostic pop
}

static int __pthread_fchdir(int fd) {
#pragma clang diagnostic push
#pragma clang diagnostic ignored "-Wdeprecated"
  return syscall(SYS___pthread_fchdir, fd);
#pragma clang diagnostic pop
}


CAMLprim value spawn_is_osx()
{
  return Val_true;
}

#else

CAMLprim value spawn_is_osx()
{
  return Val_false;
}

#endif

#if !defined(_WIN32)

# if defined(USE_POSIX_SPAWN)
#  include <spawn.h>

#  if !defined(__APPLE__)
#   define posix_spawn_file_actions_addchdir_np(...)  ENOSYS
#   define posix_spawn_file_actions_addfchdir_np(...) ENOSYS
#  endif
# endif

#include <assert.h>
#include <string.h>
#if !defined(__CYGWIN__) && !defined(__HAIKU__)
#include <sys/syscall.h>
#endif
#include <sys/types.h>
#include <sys/wait.h>
#include <unistd.h>
#include <fcntl.h>
#include <limits.h>
#include <pthread.h>
#include <signal.h>

/* +-----------------------------------------------------------------+
   | pipe2                                                           |
   +-----------------------------------------------------------------+ */

#if defined(__APPLE__) || defined(__HAIKU__)

/* vfork(2) is deprecated on macOS >= 12, so we use fork(2) instead. */
# if defined(__MAC_OS_X_VERSION_MAX_ALLOWED)
#  if __MAC_OS_X_VERSION_MAX_ALLOWED >= 120000
#   define vfork fork
#  endif
# endif

static int safe_pipe(int fd[2])
{
  int i;
  if (pipe(fd) == -1) return -1;
  for (i = 0; i < 2; i++) {
    int retcode = fcntl(fd[i], F_GETFD, 0);
    if (retcode == -1 ||
        fcntl(fd[i], F_SETFD, retcode | FD_CLOEXEC) == -1) {
      int error = errno;
      close(fd[0]);
      close(fd[1]);
      errno = error;
      return -1;
    }
  }
  return 0;
}

static pthread_mutex_t safe_pipe_mutex = PTHREAD_MUTEX_INITIALIZER;

#define enter_safe_pipe_section() pthread_mutex_lock(&safe_pipe_mutex)
#define leave_safe_pipe_section() pthread_mutex_unlock(&safe_pipe_mutex)

CAMLprim value spawn_pipe()
{
  int fd[2];
  int ret;
  value res;
  caml_enter_blocking_section();
  enter_safe_pipe_section();
  ret = safe_pipe(fd);
  leave_safe_pipe_section();
  caml_leave_blocking_section();
  if (ret == -1)
    uerror("pipe", Nothing);
  res = caml_alloc_small(2, 0);
  Field(res, 0) = Val_int(fd[0]);
  Field(res, 1) = Val_int(fd[1]);
  return res;
}

#else

#define enter_safe_pipe_section()
#define leave_safe_pipe_section()

static int safe_pipe(int fd[2])
{
  return pipe2(fd, O_CLOEXEC);
}

CAMLprim value spawn_pipe()
{
  int fd[2];
  value res;

  if (safe_pipe(fd) == -1) uerror("safe_pipe", Nothing);

  res = caml_alloc_small(2, 0);
  Field(res, 0) = Val_int(fd[0]);
  Field(res, 1) = Val_int(fd[1]);

  return res;
}

#endif

/* +-----------------------------------------------------------------+
   | Code executed in the child                                      |
   +-----------------------------------------------------------------+ */

enum error_arg { NOTHING, CWD, PROG };

/* Structure used to communicate errors from the child to the
   parent. */
struct subprocess_failure {
  /* Value of [errno]. */
  int error;
  /* System call that failed */
  char function[32];
  /* What to pass as third argument of the Unix_error exception. */
  enum error_arg arg;
};

/* Compile time asserts as described here:

   http://stackoverflow.com/questions/807244/c-compiler-asserts-how-to-implement
*/
#define CASSERT(predicate) _impl_CASSERT_LINE(predicate,__LINE__,__FILE__)
#define _impl_PASTE(a,b) a##b
#define _impl_CASSERT_LINE(predicate, line, file)                       \
  typedef char __attribute__((unused))                                  \
  _impl_PASTE(assertion_failed_##file##_,line)[2*!!(predicate)-1];

/* Fill a [subprocess_failure] structure. */
static void set_error(struct subprocess_failure *fp,
                      int error,
                      char *function,
                      enum error_arg error_arg)
{
  size_t len = strlen(function);
  assert(len + 1 <= sizeof(fp->function));
  memset(fp, 0, sizeof(*fp));
  memcpy(fp->function, function, len + 1);
  fp->error = error;
  fp->arg   = error_arg;
}

/* Report an error to the parent. Use the current value of [errno] as
   error number. */
static void subprocess_failure(int failure_fd,
                               char *function,
                               enum error_arg error_arg)
{
  struct subprocess_failure failure;
  sigset_t sigset;
  ssize_t written;

#ifdef PIPE_BUF
  CASSERT(sizeof(failure) < PIPE_BUF)
#else
  CASSERT(sizeof(failure) < _POSIX_PIPE_BUF)
#endif

  set_error(&failure, errno, function, error_arg);

  /* Block all signals to avoid being interrupted in write.
     Although most of the call sites of [subprocess_failure] already block
     signals, the one after the [exec] might not. */
  sigfillset(&sigset);
  pthread_sigmask(SIG_SETMASK, &sigset, NULL);

  /* Write is atomic as buffer is smaller than PIPE_BUF
     (required by POSIX.1-2001, as claimed in [man 7 pipe]).

     We only store the result of [write] to avoid a warning.
 */
  written = write(failure_fd, &failure, sizeof(failure));
  (void)written;
  _exit(127);
}

/* same as [dup] but ensures the result is -1 or >= 3. */
static int safe_dup(int fd)
{
  int new_fd = dup(fd);
  if (new_fd == -1 || new_fd >= 3)
    return new_fd;
  else {
    int result = safe_dup(fd);
    close(new_fd);
    return result;
  }
}

/* same as [safe_dup] but writes errors to a file descriptor,
   as from a subprocess. */
static int safe_dup_failure_fd(int failure_fd, int fd)
{
  int new_fd = safe_dup(fd);
  if (new_fd == -1) subprocess_failure(failure_fd, "dup", NOTHING);
  return new_fd;
}

enum working_dir_kind { PATH, FD, INHERIT };

struct spawn_info {
  char **env; /* can be 0, in which case the current environment is used */
  enum working_dir_kind cwd_kind;
  union {
    int fd;
    char *path;
  } cwd; /* Only filled if [cwd_kind != INHERIT] */
  char *prog;
  char **argv;
  int std_fds[3];
  int set_pgid;
  pid_t pgid;
  sigset_t child_sigmask;
};

static void subprocess(int failure_fd, struct spawn_info *info)
{
  int i, fd, tmp_fds[3];
  struct sigaction sa;

  if (info->set_pgid) {
    if (setpgid(0, info->pgid) == -1) {
      subprocess_failure(failure_fd, "setpgid", NOTHING);
      return;
    }
  }

  /* Restore all signals to their default behavior before setting the
     desired signal mask for the subprocess to avoid invoking handlers
     from the parent */
  sa.sa_handler = SIG_DFL;
  sigemptyset(&sa.sa_mask);
  sa.sa_flags = 0;
  /* Ignore errors as there is no interesting way it can fail. */
  for (i = 1; i < NSIG; i++) sigaction(i, &sa, NULL);

  switch (info->cwd_kind) {
    case INHERIT: break;
    case PATH:
      if (chdir(info->cwd.path) == -1)
        subprocess_failure(failure_fd, "chdir", CWD);
      break;
    case FD:
      if (fchdir(info->cwd.fd) == -1)
        subprocess_failure(failure_fd, "fchdir", NOTHING);
      close(info->cwd.fd);
      break;
  }

  /* Use temporary file descriptors for redirections to avoid problems
     when redirecting stdout to stderr for instance. */

  for (fd = 0; fd < 3; fd++)
    tmp_fds[fd] = safe_dup_failure_fd(failure_fd, info->std_fds[fd]);

  for (fd = 0; fd < 3; fd++)
    close(info->std_fds[fd]);

  for (fd = 0; fd < 3; fd++) {
    /* here we rely on [dup2] clearing the O_CLOEXEC flag */
    if (dup2(tmp_fds[fd], fd) == -1)
      subprocess_failure(failure_fd, "dup2", NOTHING);
    close(tmp_fds[fd]);
  }

  pthread_sigmask(SIG_SETMASK, &info->child_sigmask, NULL);

  execve(info->prog, info->argv, info->env);
  subprocess_failure(failure_fd, "execve", PROG);
}

/* +-----------------------------------------------------------------+
   | Parent code                                                     |
   +-----------------------------------------------------------------+ */

/* Raise a Unix error based on the contents of a [subprocess_failure] structure
   (and some context arguments). */
void raise_subprocess_failure(struct subprocess_failure* failure,
                              value v_cwd,
                              value v_prog)
{
  value arg = Nothing;
  switch (failure->arg) {
    case NOTHING: arg = Nothing;         break;
    case CWD    : arg = Field(v_cwd, 0); break;
    case PROG   : arg = v_prog;          break;
  }
  assert(memchr(failure->function, 0, sizeof(failure->function)));
  unix_error(failure->error, failure->function, arg);
}

/* Convert a [string list] into a NULL terminated array of C
   strings.

   We don't reuse the [cstringvect] function from [unix_support.h] as
   it doesn't copy the strings in the array.
*/
static char **alloc_string_vect(value v)
{
  char **result;
  mlsize_t count, i, full_size;
  value x;
  char *ptr;

  count = 0;
  full_size = sizeof(char*);
  for (x = v; Is_block(x); x = Field(x, 1)) {
    count++;
    full_size += sizeof(char*) + caml_string_length(Field(x, 0)) + 1;
  }

  /* Allocate the array of pointers followed by the space to copy the
     strings as one block. */
  result = (char**)malloc(full_size);
  if (result == NULL) caml_raise_out_of_memory();

  ptr = ((char*)result) + (sizeof(char*) * (count + 1));
  for (x = v, i = 0; Is_block(x); x = Field(x, 1), i++) {
    value v_str = Field(x, 0);
    mlsize_t len = caml_string_length(v_str) + 1;
    memcpy(ptr, String_val(v_str), len);
    result[i] = ptr;
    ptr += len;
  }
  result[count] = NULL;

  return result;
}

static char **copy_c_string_array(char ** strings)
{
  char **result;
  size_t count, i, full_size;
  char *ptr;

  count = 0;
  full_size = sizeof(char*);
  while (strings[count] != 0) {
    full_size += sizeof(char*) + strlen(strings[count]) + 1;
    count++;
  }

  /* Allocate the array of pointers followed by the space to copy the
     strings as one block. */
  result = (char**)malloc(full_size);
  if (result == NULL) caml_raise_out_of_memory();

  ptr = (char*)(result + count + 1);
  for (i = 0; i < count; i++) {
    size_t len = strlen(strings[i]) + 1;
    memcpy(ptr, strings[i], len);
    result[i] = ptr;
    ptr += len;
  }
  result[count] = NULL;

  return result;
}

static void free_spawn_info(struct spawn_info *info)
{
  if (info->cwd_kind == PATH) free(info->cwd.path);
  free(info->prog);
  free(info->argv);
  free(info->env);
}

enum caml_unix_sigprocmask_command {
  CAML_SIG_SETMASK,
  CAML_SIG_BLOCK,
  CAML_SIG_UNBLOCK,
};

/* Initializes all fields of `*info` except for `info->child_sigmask`,
   which must be initalized by `init_spawn_info_sigmask` (below). */
static void init_spawn_info(struct spawn_info *info,
                            value v_env,
                            value v_cwd,
                            value v_prog,
                            value v_argv,
                            value v_stdin,
                            value v_stdout,
                            value v_stderr,
                            value v_setpgid,
                            value v_sigprocmask)
{
  extern char ** environ;

  info->std_fds[0] = Int_val(v_stdin);
  info->std_fds[1] = Int_val(v_stdout);
  info->std_fds[2] = Int_val(v_stderr);

  if (Is_long(v_cwd)) {
    assert(v_cwd == Val_long(0));
    info->cwd_kind = INHERIT;
  } else {
    switch (Tag_val(v_cwd)) {
      case 0: /* Path of string */
        assert (Tag_val(Field(v_cwd, 0)) == String_tag);
        info->cwd_kind = PATH;
        info->cwd.path = strdup(String_val(Field(v_cwd, 0)));
        if (info->cwd.path == NULL) caml_raise_out_of_memory();
        break;
      case 1: /* Fd of Unix.file_descr */
        assert (Is_long(Field(v_cwd, 0)));
        info->cwd_kind = FD;
        info->cwd.fd = Int_val(Field(v_cwd, 0));
        break;
      default:
        assert(0);
    }
  }

  info->prog = strdup(String_val(v_prog));
  if (info->prog == NULL) caml_raise_out_of_memory();
  info->argv = alloc_string_vect(v_argv);
  info->env =
    Is_block(v_env) ?
    alloc_string_vect(Field(v_env, 0)) : copy_c_string_array(environ);
  info->set_pgid = Is_block(v_setpgid);
  info->pgid =
    Is_block(v_setpgid) ?
    Long_val(Field(v_setpgid, 0)) : 0;

  if (v_sigprocmask == Val_long(0)) {
    sigemptyset(&info->child_sigmask);
  } else {
    v_sigprocmask = Field(v_sigprocmask, 0);
    value v_sigprocmask_command = Field(v_sigprocmask, 0);
    enum caml_unix_sigprocmask_command sigprocmask_command = Long_val(v_sigprocmask_command);

    switch (sigprocmask_command) {
      case CAML_SIG_SETMASK:
        sigemptyset(&info->child_sigmask);
        break;

      case CAML_SIG_BLOCK:
      case CAML_SIG_UNBLOCK:
        pthread_sigmask(SIG_SETMASK, NULL, &info->child_sigmask);
        break;

      default:
        caml_failwith("Unknown sigprocmask action");
    }

    value v_signals_list = Field(v_sigprocmask, 1);
    for (; v_signals_list != Val_emptylist;
         v_signals_list = Field(v_signals_list, 1)) {
      int signal = caml_convert_signal_number(Long_val(Field(v_signals_list, 0)));
      switch (sigprocmask_command) {
        case CAML_SIG_SETMASK:
        case CAML_SIG_BLOCK:
          sigaddset(&info->child_sigmask, signal);
          break;

        case CAML_SIG_UNBLOCK:
          sigdelset(&info->child_sigmask, signal);
          break;

        default:
          assert(0);
      }
    }
  }
}

#if defined(USE_POSIX_SPAWN)

CAMLprim value spawn_unix(value v_env,
                          value v_cwd,
                          value v_prog,
                          value v_argv,
                          value v_stdin,
                          value v_stdout,
                          value v_stderr,
                          value v_use_vfork,
                          value v_setpgid,
                          value v_sigprocmask)
{
  CAMLparam4(v_env, v_cwd, v_prog, v_argv);
  CAMLlocal1(e_arg);
  e_arg = Nothing;

  pid_t pid;
  int tmp_fds[3] = {0};  // invariant: initialized > 2

  int e_error;
  char *e_function = NULL;

  posix_spawn_file_actions_t actions;
  if (posix_spawn_file_actions_init(&actions)) {
    e_function = "posix_spawn_file_actions_init";
    goto cleanup;
  }

  posix_spawnattr_t attr;
  if (posix_spawnattr_init(&attr)) {
    e_function = "posix_spawnattr_init";
    goto cleanup;
  }

  struct spawn_info info;
  init_spawn_info(&info, v_env, v_cwd, v_prog, v_argv,
                  v_stdin, v_stdout, v_stderr, v_setpgid, v_sigprocmask);

  short attr_flags = POSIX_SPAWN_SETSIGMASK;
  if (info.set_pgid) attr_flags |= POSIX_SPAWN_SETPGROUP;
  e_error = posix_spawnattr_setflags(&attr, attr_flags);
  if (e_error) {
    e_function = "posix_spawnattr_setflags";
    goto cleanup;
  }

  e_error = posix_spawnattr_setsigmask(&attr, &info.child_sigmask);
  if (e_error) {
    e_function = "posix_spawnattr_setsigmask";
    goto cleanup;
  }

  if (info.set_pgid) {
    e_error = posix_spawnattr_setpgroup(&attr, info.pgid);
    if (e_error) {
      e_function = "posix_spawnattr_setpgroup";
      goto cleanup;
    }
  }

  for (int fd = 0; fd < 3; fd++) {
    int tmp_fd = tmp_fds[fd] = safe_dup(info.std_fds[fd]);
    if (tmp_fd == -1) {
      e_error = errno;
      e_function = "dup";
      goto cleanup;
    }

    e_error = posix_spawn_file_actions_adddup2(&actions, tmp_fd, fd);
    if (e_error) {
      e_function = "posix_spawn_file_actions_adddup2";
      goto cleanup;
    }

    e_error = posix_spawn_file_actions_addclose(&actions, tmp_fd);
    if (e_error) {
      e_function = "posix_spawn_file_actions_addclose";
      goto cleanup;
    }
  }

  switch (info.cwd_kind) {
    case INHERIT: break;
    case PATH:
      e_error = __pthread_chdir(info.cwd.path);
      if (e_error) {
        e_function = "__pthread_chdir";
        goto cleanup;
      }
      break;
    case FD:
      e_error = __pthread_fchdir(info.cwd.fd);
      if (e_error) {
        e_function = "__pthread_fchdir";
        goto cleanup;
      }
  }
  caml_enter_blocking_section();
  e_error = posix_spawn(&pid, info.prog,
                        &actions, &attr,
                        info.argv, info.env);
  if (info.cwd_kind != INHERIT) {
    /* go back to inheriting the process cwd */
    __pthread_fchdir(-1);
  }
  caml_leave_blocking_section();

  if (e_error) {
    e_function = "posix_spawn";
    goto cleanup;
  }

 cleanup:
  for (int fd = 0; fd < 3; fd++)
    if (tmp_fds[fd] > 2)
      close(tmp_fds[fd]);

  free_spawn_info(&info);
  posix_spawnattr_destroy(&attr);
  posix_spawn_file_actions_destroy(&actions);

  if (e_function) {
    unix_error(e_error, e_function, e_arg);
  }

  CAMLreturn(Val_int(pid));
}

#else

CAMLprim value spawn_unix(value v_env,
                          value v_cwd,
                          value v_prog,
                          value v_argv,
                          value v_stdin,
                          value v_stdout,
                          value v_stderr,
                          value v_use_vfork,
                          value v_setpgid,
                          value v_sigprocmask)
{
  CAMLparam4(v_env, v_cwd, v_prog, v_argv);
  pid_t ret;
  struct spawn_info info;
  int result_pipe[2];
  int cancel_state;
  sigset_t sigset;
  sigset_t saved_procmask;
  struct subprocess_failure failure;
  int got_error = 0;
  int errno_after_forking = 0;
  int status;

  init_spawn_info(&info, v_env, v_cwd, v_prog, v_argv,
                  v_stdin, v_stdout, v_stderr, v_setpgid, v_sigprocmask);

  caml_enter_blocking_section();
  enter_safe_pipe_section();

  /* Pipe used by the child to send errors to the parent. */
  if (safe_pipe(result_pipe) == -1) {
    int error = errno;
    leave_safe_pipe_section();
    caml_leave_blocking_section();
    free_spawn_info(&info);
    unix_error(error, "pipe", Nothing);
  }

  /* Block signals and thread cancellation. When using vfork, the
     child might share the signal handlers.

     It's not clear that we need the call to [pthread_setcancelstate],
     but implementations of posix_spawn based on vfork are doing this.

     For instance:
     http://git.musl-libc.org/cgit/musl/tree/src/process/posix_spawn.c

     On android, pthread_cancel is not implemented, it is typically
     patched out or in certain cases reimplemented with atomic_flags
     https://github.com/search?q=org%3Atermux+pthread_setcancelstate+language%3ADiff&type=code&l=Diff
  */

  #if !defined(__ANDROID__)
  pthread_setcancelstate(PTHREAD_CANCEL_DISABLE, &cancel_state);
  #endif
  sigfillset(&sigset);
  pthread_sigmask(SIG_SETMASK, &sigset, &saved_procmask);

  ret = Bool_val(v_use_vfork) ? vfork() : fork();

  if (ret == 0) {
    close(result_pipe[0]);
    subprocess(result_pipe[1], &info);
  }
  errno_after_forking = errno;

  leave_safe_pipe_section();
  free_spawn_info(&info);
  close(result_pipe[1]);

  got_error = 0;
  if (ret == -1) {
    got_error = 1;
    set_error(&failure, errno_after_forking, "vfork", NOTHING);
  } else {
    intnat res = read(result_pipe[0], &failure, sizeof(failure));
    /* If the sub-process exec successfully, the write end of the
       error pipe is closed (as it has the [O_CLOEXEC] flag set) and
       [read] returns [0].

       If it returns non-zero, it means something went wrong in the
       sub-process and it wrote a [subprocess_failure] structure on
       the pipe. */
    if (res != 0) {
      got_error = 1;
      if (res != sizeof(failure)) {
        /* It's not clear this can happen, but just to be on the safe side */
        set_error(&failure,
                  (res == -1) ? errno : EINVAL,
                  "read",
                  NOTHING);
      };
      /* If [read] did fail for some reason then we might be stuck
         here for a while. Other implementation of posix_spawn just
         assume that [read(...) != sizeof(failure)] is a success. */
      if (got_error) waitpid(ret, &status, 0);
    }
  }

  close(result_pipe[0]);
  pthread_sigmask(SIG_SETMASK, &saved_procmask, NULL);
  #if !defined(__ANDROID__)
  pthread_setcancelstate(cancel_state, NULL);
  #endif

  caml_leave_blocking_section();

  if (got_error) {
    raise_subprocess_failure(&failure, v_cwd, v_prog);
  }

  CAMLreturn(Val_int(ret));
}

#endif

CAMLprim value spawn_windows(value v_env,
                             value v_cwd,
                             value v_prog,
                             value v_cmdline,
                             value v_stdin,
                             value v_stdout,
                             value v_stderr)
{
  (void)v_env;
  (void)v_cwd;
  (void)v_prog;
  (void)v_cmdline;
  (void)v_stdin;
  (void)v_stdout;
  (void)v_stderr;
  unix_error(ENOSYS, "spawn_windows", Nothing);
}

#else

CAMLprim value spawn_unix(value v_env,
                          value v_cwd,
                          value v_prog,
                          value v_argv,
                          value v_stdin,
                          value v_stdout,
                          value v_stderr,
                          value v_use_vfork,
                          value v_setpgid,
                          value v_sigprocmask)
{
  (void)v_env;
  (void)v_cwd;
  (void)v_prog;
  (void)v_argv;
  (void)v_stdin;
  (void)v_stdout;
  (void)v_stderr;
  (void)v_use_vfork;
  (void)v_setpgid;
  (void)v_sigprocmask;
  unix_error(ENOSYS, "spawn_unix", Nothing);
}

static BOOL dup2_and_clear_close_on_exec(value fd, HANDLE *res)
{
  return DuplicateHandle(GetCurrentProcess(), Handle_val(fd),
                         GetCurrentProcess(), res,
                         0L,
                         TRUE,
                         DUPLICATE_SAME_ACCESS);
}

static void close_std_handles(STARTUPINFO *si)
{
  if (si->hStdInput  != NULL) CloseHandle(si->hStdInput );
  if (si->hStdOutput != NULL) CloseHandle(si->hStdOutput);
  if (si->hStdError  != NULL) CloseHandle(si->hStdError );
}

CAMLprim value spawn_windows(value v_env,
                             value v_cwd,
                             value v_prog,
                             value v_cmdline,
                             value v_stdin,
                             value v_stdout,
                             value v_stderr)
{
  STARTUPINFO si;
  PROCESS_INFORMATION pi;
  WCHAR *prog;
  WCHAR *cmdline;
  WCHAR *env = NULL;
  WCHAR *cwd = NULL;
  BOOL result;

  ZeroMemory(&si, sizeof(si));
  ZeroMemory(&pi, sizeof(pi));

  if (!dup2_and_clear_close_on_exec(v_stdin , &si.hStdInput ) ||
      !dup2_and_clear_close_on_exec(v_stdout, &si.hStdOutput) ||
      !dup2_and_clear_close_on_exec(v_stderr, &si.hStdError )) {
    caml_win32_maperr(GetLastError());
    close_std_handles(&si);
    uerror("DuplicateHandle", Nothing);
  }

  prog = caml_stat_strdup_to_utf16(String_val(v_prog));
  cmdline = caml_stat_strdup_to_utf16(String_val(v_cmdline));

  if (Is_block(v_env)) {
    v_env = Field(v_env, 0);
    mlsize_t len = caml_string_length(v_env);
    int size = caml_win32_multi_byte_to_wide_char(String_val(v_env), len, NULL, 0);
    env = caml_stat_alloc(size * sizeof(WCHAR));
    caml_win32_multi_byte_to_wide_char(String_val(v_env), len, env, size);
  }

  if (Is_block(v_cwd))
    cwd = caml_stat_strdup_to_utf16(String_val(Field(v_cwd, 0)));

  si.cb = sizeof(si);
  si.dwFlags = STARTF_USESTDHANDLES;

  result =
    CreateProcess(prog, cmdline, NULL, NULL, TRUE, CREATE_UNICODE_ENVIRONMENT,
                  env, cwd, &si, &pi);

  caml_stat_free(prog);
  caml_stat_free(cmdline);
  caml_stat_free(env);
  caml_stat_free(cwd);

  if (!result) {
    caml_win32_maperr(GetLastError());
    close_std_handles(&si);
    uerror("CreateProcess", Nothing);
  }

  close_std_handles(&si);
  CloseHandle(pi.hThread);

  return Val_long(pi.hProcess);
}

CAMLprim value spawn_pipe()
{
  unix_error(ENOSYS, "spawn_pipe", Nothing);
}

#endif

CAMLprim value spawn_unix_byte(value * argv)
{
  return spawn_unix(argv[0],
                    argv[1],
                    argv[2],
                    argv[3],
                    argv[4],
                    argv[5],
                    argv[6],
                    argv[7],
                    argv[8],
                    argv[9]);
}

CAMLprim value spawn_windows_byte(value * argv)
{
  return spawn_windows(argv[0],
                       argv[1],
                       argv[2],
                       argv[3],
                       argv[4],
                       argv[5],
                       argv[6]);
}
