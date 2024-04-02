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

CAMLprim value stdune_sendfile(value v_in, value v_out, value v_size) {
  (void)v_in;
  (void)v_out;
  (void)v_size;
  caml_failwith("sendfile: linux");
}

#elif __linux__

#include <caml/threads.h>
#include <caml/unixsupport.h>

#include <sys/sendfile.h>
#include <unistd.h>

#define FD_val(value) Int_val(value)

CAMLprim value stdune_copyfile(value v_from, value v_to) {
  (void)v_from;
  (void)v_to;
  caml_failwith("copyfile: only on macos");
}

static int dune_sendfile(int in, int out, size_t length) {
  ssize_t ret;
  while (length > 0) {
    ret = sendfile(out, in, NULL, length);
    if (ret < 0) {
      return ret;
    }
    length = length - ret;
  }
  return length;
}

CAMLprim value stdune_sendfile(value v_in, value v_out, value v_size) {
  CAMLparam3(v_in, v_out, v_size);
  caml_release_runtime_system();
  /* TODO Use copy_file_range once we have a good mechanism to test for its
   * existence */
  int ret = dune_sendfile(FD_val(v_in), FD_val(v_out), Long_val(v_size));
  caml_acquire_runtime_system();
  if (ret < 0) {
    uerror("sendfile", Nothing);
  }
  CAMLreturn(Val_unit);
}

#else

CAMLprim value stdune_sendfile(value v_in, value v_out, value v_size) {
  (void)v_in;
  (void)v_out;
  (void)v_size;
  caml_failwith("sendfile: linux");
}

CAMLprim value stdune_copyfile(value v_from, value v_to) {
  (void)v_from;
  (void)v_to;
  caml_failwith("copyfile: only on macos");
}

#endif
