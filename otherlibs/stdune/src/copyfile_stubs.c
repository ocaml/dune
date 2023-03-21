#include <caml/fail.h>
#include <caml/memory.h>
#include <caml/mlvalues.h>

#if defined(__APPLE__)
#define _DARWIN_C_SOURCE

#include <caml/alloc.h>
#include <caml/threads.h>
#include <caml/unixsupport.h>

#include <copyfile.h>
#include <errno.h>
#include <stdlib.h>
#include <string.h>
#include <sys/syslimits.h>

CAMLprim value stdune_copyfile(value v_from, value v_to) {
  CAMLparam2(v_from, v_to);
  caml_unix_check_path(v_from, "copyfile");
  caml_unix_check_path(v_to, "copyfile");
  char from[PATH_MAX];
  char to[PATH_MAX];
  char real_from[PATH_MAX];
  int from_len = caml_string_length(v_from);
  int to_len = caml_string_length(v_to);
  memcpy(from, String_val(v_from), from_len);
  memcpy(to, String_val(v_to), to_len);
  from[from_len] = '\0';
  to[to_len] = '\0';

  caml_release_runtime_system();
  /* clonefile doesn't follow symlinks automatically */
  char *realpath_result = realpath(from, real_from);
  if (realpath_result == NULL) {
    caml_acquire_runtime_system();
    uerror("realpath", v_from);
  }
  /* nor does it automatically overwrite the target */
  int ret = unlink(to);
  if (ret < 0 && errno != ENOENT) {
    caml_acquire_runtime_system();
    uerror("unlink", v_to);
  }
  ret = copyfile(real_from, to, NULL, COPYFILE_CLONE);
  caml_acquire_runtime_system();
  if (ret < 0) {
    uerror("copyfile", v_to);
  }
  CAMLreturn(Val_unit);
}

CAMLprim value stdune_is_darwin(value v_unit) {
  CAMLparam1(v_unit);
  CAMLreturn(Val_true);
}

#else

CAMLprim value stdune_copyfile(value v_from, value v_to) {
  caml_failwith("copyfile: only on macos");
}

CAMLprim value stdune_is_darwin(value v_unit) {
  CAMLparam1(v_unit);
  CAMLreturn(Val_false);
}

#endif
