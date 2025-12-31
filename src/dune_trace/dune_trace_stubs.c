#include <caml/alloc.h>
#include <caml/threads.h>
#include <caml/fail.h>
#include <caml/memory.h>
#include <caml/mlvalues.h>
#include <caml/bigarray.h>
#include <caml/unixsupport.h>

#ifdef _MSC_VER
#include <io.h>
typedef SSIZE_T	ssize_t;
#else
#include <unistd.h>
#endif

#if OCAML_VERSION_MAJOR < 5
#define caml_uerror uerror
#endif

CAMLprim value dune_trace_write(value v_fd, value v_buf, value v_offset,
                                value v_length) {
  CAMLparam4(v_fd, v_buf, v_offset, v_length);
#ifdef _WIN32
  int fd = win_CRT_fd_of_filedescr(v_fd);
#else
  int fd = Int_val(v_fd);
#endif
  intnat offset = Long_val(v_offset);
  intnat length = Long_val(v_length);
  void *buf = Caml_ba_data_val(v_buf);
  ssize_t written;

  caml_release_runtime_system();
  written = write(fd, (char *)buf + offset, length);
  caml_acquire_runtime_system();

  if (written == -1) {
    caml_uerror("dune_trace_write", Nothing);
  }

  CAMLreturn(Val_long(written));
}

#if defined(__APPLE__)
#include <libproc.h>
#include <sys/errno.h>

#define MAX_FDS 1024 // should be enough for anybody

CAMLprim value dune_trace_open_fds(value v_pid) {
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

CAMLprim value dune_trace_available(value unit) {
  CAMLparam1(unit);
  CAMLreturn(Val_true);
}

#else

CAMLprim value dune_trace_available(value unit) {
  CAMLparam1(unit);
  CAMLreturn(Val_false);
}

CAMLprim value dune_trace_open_fds(value v_pid) {
  (void)v_pid;
  caml_failwith("function is available only on macos");
}

#endif
