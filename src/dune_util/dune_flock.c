#include <caml/memory.h>
#include <caml/mlvalues.h>
#include <caml/threads.h>
#include <caml/unixsupport.h>

#ifdef _WIN32

#define FD_val(value) Handle_val(value)

CAMLprim value dune_flock_lock(value v_fd, value v_block, value v_exclusive) {
  CAMLparam3(v_fd, v_block, v_exclusive);
  OVERLAPPED overlapped = { 0 };
  DWORD ok, dwFlags = 0;
  if (Bool_val(v_exclusive)) {
    dwFlags |= LOCKFILE_EXCLUSIVE_LOCK;
  }
  if (!Bool_val(v_block)) {
    dwFlags |= LOCKFILE_FAIL_IMMEDIATELY;
  }
  caml_release_runtime_system();
  ok = LockFileEx(FD_val(v_fd), dwFlags, 0, MAXDWORD, MAXDWORD, &overlapped);
  caml_acquire_runtime_system();
  if (!ok) {
    win32_maperr(GetLastError());
    uerror("LockFileEx", Nothing);
  }
  CAMLreturn(Val_unit);
}

CAMLprim value dune_flock_unlock(value v_fd) {
  CAMLparam1(v_fd);
  OVERLAPPED overlapped = { 0 };
  DWORD ok;
  caml_release_runtime_system();
  ok = UnlockFileEx(FD_val(v_fd), 0, MAXDWORD, MAXDWORD, &overlapped);
  caml_acquire_runtime_system();
  if (!ok) {
    win32_maperr(GetLastError());
    uerror("UnlockFileEx", Nothing);
  }
  CAMLreturn(Val_unit);
}

#else /* _WIN32 */

#include <sys/file.h>

#define FD_val(value) Int_val(value)

CAMLprim value dune_flock_lock(value v_fd, value v_block, value v_exclusive) {
  CAMLparam3(v_fd, v_block, v_exclusive);
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
}

CAMLprim value dune_flock_unlock(value v_fd) {
  CAMLparam1(v_fd);
  caml_release_runtime_system();
  int ret = flock(FD_val(v_fd), LOCK_UN);
  caml_acquire_runtime_system();
  if (ret == 0) {
    CAMLreturn(Val_unit);
  } else {
    uerror("flock", Nothing);
  }
}

#endif /* _WIN32 */
