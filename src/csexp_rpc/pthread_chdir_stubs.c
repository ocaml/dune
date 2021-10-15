#include <caml/mlvalues.h>
#include <caml/memory.h>
#include <caml/alloc.h>
#include <caml/custom.h>
#include <caml/fail.h>
#include <caml/signals.h>
#include <caml/callback.h>
#include <caml/unixsupport.h>

#if defined(__APPLE__)

#include <sys/syscall.h>
#include <unistd.h>
#include <fcntl.h>

CAMLprim value dune_pthread_chdir_is_osx(value unit)
{
  CAMLparam1(unit);
  CAMLreturn(Val_true);
}


#ifndef SYS___pthread_chdir
# define SYS___pthread_chdir 348
#endif

int __pthread_chdir(const char *path)
{
#pragma clang diagnostic push
#pragma clang diagnostic ignored "-Wdeprecated"
  return syscall(SYS___pthread_chdir, path);
#pragma clang diagnostic pop
}

CAMLprim value dune_pthread_chdir(value dir) {
  CAMLparam1(dir);
  if (__pthread_chdir(String_val(dir))) {
    uerror("__pthread_chdir", Nothing);
  } 
  CAMLreturn (Val_unit); 
}

#else

CAMLprim value dune_pthread_chdir_is_osx(value unit)
{
  CAMLparam1(unit);
  CAMLreturn(Val_false);
}

CAMLprim value dune_pthread_chdir(value unit) {
  CAMLparam1(unit);
  caml_invalid_argument("__pthread_chdir not implemented");
  CAMLreturn (Val_unit); 
}

#endif
