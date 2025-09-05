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
#include <sys/utsname.h>
#include <linux/version.h>
#include <dlfcn.h>
#include <stdio.h>

#define FD_val(value) Int_val(value)

CAMLprim value stdune_copyfile(value v_from, value v_to) {
  (void)v_from;
  (void)v_to;
  caml_failwith("copyfile: only on macos");
}

static ssize_t dune_sendfile(int in, int out, size_t length) {
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

/* The second and fourth arguments are type-incorrect, but as they are unused,
   this is OK */
typedef ssize_t (*copy_file_range_t)(int, void *, int, void *, size_t, unsigned int);

static copy_file_range_t copy_file_range_fn = NULL;

static ssize_t dune_copy_file_range(int in, int out, size_t length) {
  ssize_t ret;
  while (length > 0) {
    ret = copy_file_range_fn(in, NULL, out, NULL, length, 0);
    if (ret < 0) {
      return dune_sendfile(in, out, length);
    }
    length = length - ret;
  }
  return length;
}

static int kernel_version(void) {
  struct utsname uts;
  int major, minor, patch;

  if (uname(&uts) < 0)
    return -1;

  if (sscanf(uts.release, "%d.%d.%d", &major, &minor, &patch) != 3)
    return -1;

  return KERNEL_VERSION(major, minor, patch);
}

CAMLprim value stdune_sendfile(value v_in, value v_out, value v_size) {
  static ssize_t (*dune_copyfile)(int, int, size_t) = NULL;
  if (dune_copyfile == NULL) {
    if (kernel_version() < KERNEL_VERSION(5, 19, 0) ||
        (copy_file_range_fn = (copy_file_range_t)dlsym(NULL, "copy_file_range")) == NULL) {
      dune_copyfile = &dune_sendfile;
    } else {
      dune_copyfile = &dune_copy_file_range;
    }
  }
  CAMLparam3(v_in, v_out, v_size);
  caml_release_runtime_system();
  ssize_t ret = dune_copyfile(FD_val(v_in), FD_val(v_out), Long_val(v_size));
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
