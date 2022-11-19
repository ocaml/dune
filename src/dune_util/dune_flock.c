#include <caml/memory.h>
#include <caml/mlvalues.h>
#include <caml/threads.h>
#include <caml/unixsupport.h>

#ifndef _WIN32
#include <sys/file.h>
#endif

#define FD_val(value) Int_val(value)

CAMLprim value dune_flock_lock(value v_fd, value v_block, value v_exclusive) {
#ifdef _WIN32
  caml_failwith("no flock on win32");
  return Val_unit;
#else
  CAMLparam2(v_fd, v_block);
  int flags = 0;
  if (Bool_val(v_exclusive)) {
    flags |= LOCK_EX;
  }
  if (!Bool_val(v_block)) {
    flags |= LOCK_NB;
  }
  caml_release_runtime_system();
  int ret = flock(FD_val(v_fd), flags);
  caml_acquire_runtime_system();
  if (ret == 0) {
    CAMLreturn(Val_unit);
  } else {
    uerror("flock", Nothing);
  }
#endif
}

CAMLprim value dune_flock_unlock(value v_fd) {
#ifdef _WIN32
  caml_failwith("no flock on win32");
  return Val_unit;
#else
  CAMLparam1(v_fd);
  caml_release_runtime_system();
  int ret = flock(FD_val(v_fd), LOCK_UN);
  caml_acquire_runtime_system();
  if (ret == 0) {
    CAMLreturn(Val_unit);
  } else {
    uerror("flock", Nothing);
  }
#endif
}
