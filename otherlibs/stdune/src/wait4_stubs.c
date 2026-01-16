#include <caml/mlvalues.h>

#ifdef _WIN32
#include <caml/fail.h>

void dune_wait4(value v_pid, value flags) {
  (void)flags;
  caml_failwith("wait4: not supported on windows");
}

#else

#ifndef Val_none
#define Val_none Val_int(0)
#endif

#include <caml/alloc.h>
#include <caml/memory.h>
#include <caml/signals.h>
#include <caml/unixsupport.h>

#include <errno.h>
#include <sys/resource.h>
#include <sys/time.h>
#include <sys/types.h>
#include <sys/wait.h>

#define TAG_WEXITED 0
#define TAG_WSIGNALED 1
#define TAG_WSTOPPED 2

static inline value caml_alloc_some_compat(value v) {
  CAMLparam1(v);
  CAMLlocal1(some);
  some = caml_alloc_small(1, 0);
  Field(some, 0) = v;
  CAMLreturn(some);
}

CAMLextern int caml_convert_signal_number(int);
CAMLextern int caml_rev_convert_signal_number(int);

static value alloc_process_status(int status) {
  value st;

  if (WIFEXITED(status)) {
    st = caml_alloc_small(1, TAG_WEXITED);
    Field(st, 0) = Val_int(WEXITSTATUS(status));
  } else if (WIFSTOPPED(status)) {
    st = caml_alloc_small(1, TAG_WSTOPPED);
    Field(st, 0) = Val_int(caml_rev_convert_signal_number(WSTOPSIG(status)));
  } else {
    st = caml_alloc_small(1, TAG_WSIGNALED);
    Field(st, 0) = Val_int(caml_rev_convert_signal_number(WTERMSIG(status)));
  }
  return st;
}

static int wait_flag_table[] = {WNOHANG, WUNTRACED};

// see https://man7.org/linux/man-pages/man2/waitpid.2.html for a description of
// possible v_pid values -1 is the one we typically use, which means wait for
// any child process
value dune_wait4(value v_pid, value flags) {
  CAMLparam2(v_pid, flags);
  CAMLlocal2(times, res);

  int status, cv_flags;
  struct timeval tp;
  cv_flags = caml_convert_flag_list(flags, wait_flag_table);
  pid_t pid = Int_val(v_pid);

  struct rusage ru;

  caml_enter_blocking_section();
  // returns the pid of the terminated process, or -1 on error
  pid = wait4(pid, &status, cv_flags, &ru);
  int wait_errno = errno;
  gettimeofday(&tp, NULL);
  caml_leave_blocking_section();
  if (pid == 0) {
    CAMLreturn(Val_none);
  } else if (pid == -1) {
    if (wait_errno == ECHILD) {
      CAMLreturn(Val_none);
    } else {
      errno = wait_errno;
      uerror("wait4", Nothing);
    }
  }

  times = caml_alloc_tuple(9);
  Store_field(times, 0, caml_copy_double(ru.ru_utime.tv_sec + ru.ru_utime.tv_usec / 1e6));
  Store_field(times, 1, caml_copy_double(ru.ru_stime.tv_sec + ru.ru_stime.tv_usec / 1e6));
  Store_field(times, 2, Val_long(ru.ru_maxrss));
  Store_field(times, 3, Val_long(ru.ru_minflt));
  Store_field(times, 4, Val_long(ru.ru_majflt));
  Store_field(times, 5, Val_long(ru.ru_inblock));
  Store_field(times, 6, Val_long(ru.ru_oublock));
  Store_field(times, 7, Val_long(ru.ru_nvcsw));
  Store_field(times, 8, Val_long(ru.ru_nivcsw));

  res = caml_alloc_tuple(4);
  Store_field(res, 0, Val_int(pid));
  Store_field(res, 1, alloc_process_status(status));
  Store_field(res, 2,
              caml_copy_double(((double)tp.tv_sec + (double)tp.tv_usec / 1e6)));
  Store_field(res, 3, times);
  CAMLreturn(caml_alloc_some_compat(res));
}

#endif
