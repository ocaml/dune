#include <caml/fail.h>
#include <caml/memory.h>
#include <caml/mlvalues.h>

#if defined(__APPLE__)
#include <libproc.h>
#include <sys/errno.h>

#define MAX_FDS 1024 // should be enough for anybody

CAMLprim value dune_stats_open_fds(value v_pid) {
  CAMLparam1(v_pid);
  pid_t pid = Int_val(v_pid);
  int size = PROC_PIDLISTFD_SIZE * MAX_FDS;
  struct proc_fdinfo fdinfo[size];
  size = proc_pidinfo(pid, PROC_PIDLISTFDS, 0, fdinfo, size);
  if (size == ENOMEM) {
    // If we ever end up eating this many fd's, correct metrics would be the
    // least of our problems
    return MAX_FDS;
  } else if (size < 0) {
    caml_failwith("proc_pidinfo failed");
  }
  int fds = size / PROC_PIDLISTFD_SIZE;
  CAMLreturn(Val_int(fds));
}

CAMLprim value dune_stats_available(value unit) {
  CAMLparam1(unit);
  CAMLreturn(Val_true);
}

#else

CAMLprim value dune_stats_available(value unit) {
  CAMLparam1(unit);
  CAMLreturn(Val_false);
}

CAMLprim value dune_stats_open_fds(value v_pid) {
  caml_failwith("function is available only on macos");
}

#endif
