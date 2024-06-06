#include <caml/mlvalues.h>

#ifdef _WIN32
#include <caml/fail.h>

void dune_wait4(value v_pid, value flags) {
  (void)flags;
  caml_failwith("wait4: not supported on windows");
}

#else

#include <caml/alloc.h>
#include <caml/memory.h>
#include <caml/signals.h>
#include <caml/unixsupport.h>

#include <sys/resource.h>
#include <sys/time.h>
#include <sys/types.h>
#include <sys/wait.h>

#define TAG_WEXITED 0
#define TAG_WSIGNALED 1
#define TAG_WSTOPPED 2

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
  gettimeofday(&tp, NULL);
  caml_leave_blocking_section();
  if (pid == -1)
    uerror("wait4", Nothing);

  times = caml_alloc_small(2 * Double_wosize, Double_array_tag);
  Store_double_field(times, 0, ru.ru_utime.tv_sec + ru.ru_utime.tv_usec / 1e6);
  Store_double_field(times, 1, ru.ru_stime.tv_sec + ru.ru_stime.tv_usec / 1e6);

  res = caml_alloc_tuple(4);
  Store_field(res, 0, Val_int(pid));
  Store_field(res, 1, alloc_process_status(status));
  Store_field(res, 2,
              caml_copy_double(((double)tp.tv_sec + (double)tp.tv_usec / 1e6)));
  Store_field(res, 3, times);
  CAMLreturn(res);
}

#endif
