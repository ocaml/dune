#include <errno.h>
#include <stdint.h>
#include <string.h>
#ifndef _WIN32
#ifndef O_CLOEXEC
#define O_CLOEXEC 0
#endif
#include <fcntl.h>
#include <sys/stat.h>
#include <unistd.h>
#endif

#include <caml/alloc.h>
#include <caml/bigarray.h>
#include <caml/custom.h>
#include <caml/fail.h>
#include <caml/memory.h>
#include <caml/mlvalues.h>
#include <caml/osdeps.h>
#include <caml/threads.h>
#include <caml/unixsupport.h>

#include "blake3.h"

#define Blake3_val(v) (*(blake3_hasher **)Data_custom_val(v))

#define BLAKE3_MINI_DIGEST_SIZE 16

static inline uint64_t digest_field(value digest, mlsize_t field) {
  uint64_t result;
  memcpy(&result, Op_val(digest) + (field * Double_wosize), sizeof(result));
  return result;
}

static inline uint64_t digest_field_for_compare(value digest, mlsize_t field) {
  uint64_t result = digest_field(digest, field);
#ifndef ARCH_BIG_ENDIAN
  result = ((result & UINT64_C(0x00ff00ff00ff00ff)) << 8) |
           ((result & UINT64_C(0xff00ff00ff00ff00)) >> 8);
  result = ((result & UINT64_C(0x0000ffff0000ffff)) << 16) |
           ((result & UINT64_C(0xffff0000ffff0000)) >> 16);
  result = (result << 32) | (result >> 32);
#endif
  return result;
}

CAMLprim value blake3_mini_digest_equal(value left, value right) {
  return Val_bool(digest_field(left, 0) == digest_field(right, 0) &&
                  digest_field(left, 1) == digest_field(right, 1));
}

CAMLprim value blake3_mini_digest_compare(value left, value right) {
  uint64_t left_field = digest_field_for_compare(left, 0);
  uint64_t right_field = digest_field_for_compare(right, 0);
  if (left_field == right_field) {
    left_field = digest_field_for_compare(left, 1);
    right_field = digest_field_for_compare(right, 1);
  }
  return Val_int((left_field > right_field) - (left_field < right_field));
}

static inline value alloc_hash(blake3_hasher *hasher) {
  /* Keep this allocation in sync with the float-only record in
     [Blake3_mini.Digest]. */
  value v_ret = caml_alloc(2 * Double_wosize, Double_array_tag);
  blake3_hasher_finalize(hasher, (uint8_t *)Op_val(v_ret),
                         BLAKE3_MINI_DIGEST_SIZE);
  return v_ret;
}

CAMLprim value blake3_mini_fd(value v_fd) {
  CAMLparam1(v_fd);
#ifdef _WIN32
  int fd = win_CRT_fd_of_filedescr(v_fd);
#else
  int fd = Int_val(v_fd);
#endif
  caml_release_runtime_system();

  blake3_hasher hasher;
  blake3_hasher_init(&hasher);

  char buffer[UNIX_BUFFER_SIZE];

  intnat bytes_read;
  while (1) {
    bytes_read = read(fd, buffer, sizeof(buffer));
    if (bytes_read == 0) {
      break;
    } else if (bytes_read < 0) {
      if (errno == EINTR)
        continue;
      caml_acquire_runtime_system();
      uerror("read", Nothing);
    } else {
      blake3_hasher_update(&hasher, buffer, bytes_read);
    }
  }

  caml_acquire_runtime_system();
  CAMLlocal1(v_ret);
  v_ret = alloc_hash(&hasher);
  CAMLreturn(v_ret);
}

CAMLprim value blake3_mini_file_with_size(value v_path) {
#ifdef _WIN32
  caml_failwith("blake3_mini_file_with_size is not implemented on Windows");
#else
  CAMLparam1(v_path);
  CAMLlocal3(v_digest, v_size, v_result);
  caml_unix_check_path(v_path, "open");
  char_os *path = caml_stat_strdup_to_os(String_val(v_path));
  blake3_hasher hasher;
  blake3_hasher_init(&hasher);
  int err = 0;
  const char *err_op = NULL;
  int fd = -1;
  intnat size = 0;

  caml_release_runtime_system();

  while (1) {
    fd = open(path, O_RDONLY | O_CLOEXEC);
    if (fd != -1 || errno != EINTR)
      break;
  }
  if (fd == -1) {
    err = errno;
    err_op = "open";
    goto done;
  }

  struct stat st;
  while (fstat(fd, &st) == -1) {
    if (errno == EINTR)
      continue;
    err = errno;
    err_op = "fstat";
    goto close;
  }
  size = st.st_size;

  char buffer[UNIX_BUFFER_SIZE];
  ssize_t bytes_read;
  while (1) {
    bytes_read = read(fd, buffer, sizeof(buffer));
    if (bytes_read == 0) {
      break;
    } else if (bytes_read < 0) {
      if (errno == EINTR)
        continue;
      err = errno;
      err_op = "read";
      break;
    } else {
      blake3_hasher_update(&hasher, buffer, bytes_read);
    }
  }

close:
  if (close(fd) == -1 && err == 0) {
    err = errno;
    err_op = "close";
  }

done:
  caml_acquire_runtime_system();
  caml_stat_free(path);
  if (err != 0) {
    errno = err;
    uerror(err_op, v_path);
  }

  v_digest = alloc_hash(&hasher);
  v_size = Val_long(size);
  v_result = caml_alloc_tuple(2);
  Store_field(v_result, 0, v_digest);
  Store_field(v_result, 1, v_size);
  CAMLreturn(v_result);
#endif
}

static void blake3_mini_finalize(value v_t) {
  blake3_hasher *hasher = Blake3_val(v_t);
  caml_stat_free(hasher);
}

static struct custom_operations blake3_mini_t_ops = {
    "blake3.mini.stream",       blake3_mini_finalize,
    custom_compare_default,     custom_hash_default,
    custom_serialize_default,   custom_deserialize_default,
    custom_compare_ext_default, custom_fixed_length_default};

CAMLprim value blake3_mini_create(value v_unit) {
  CAMLparam1(v_unit);
  CAMLlocal1(v_t);

  blake3_hasher *hasher = caml_stat_alloc(sizeof(blake3_hasher));
  blake3_hasher_init(hasher);
  v_t = caml_alloc_custom(&blake3_mini_t_ops, sizeof(blake3_hasher *), 0, 1);
  Blake3_val(v_t) = hasher;

  CAMLreturn(v_t);
}

CAMLprim value blake3_mini_reset(value v_t) {
  CAMLparam1(v_t);

  blake3_hasher *hasher = Blake3_val(v_t);
  blake3_hasher_reset(hasher);

  CAMLreturn(Val_unit);
}

CAMLprim value blake3_mini_digest(value v_t) {
  CAMLparam1(v_t);
  CAMLlocal1(v_ret);

  blake3_hasher *hasher = Blake3_val(v_t);
  v_ret = alloc_hash(hasher);

  CAMLreturn(v_ret);
}

static void blake3_mini_check_range(intnat pos, intnat len, uintnat size) {
  if (pos < 0 || len < 0 || (uintnat)pos > size ||
      (uintnat)len > size - (uintnat)pos) {
    caml_invalid_argument("Blake3_mini.feed: invalid range");
  }
}

CAMLprim value blake3_mini_feed_string(value v_t, value v_s, value v_pos,
                                       value v_len) {
  CAMLparam4(v_t, v_s, v_pos, v_len);

  blake3_hasher *hasher = Blake3_val(v_t);
  const char *s = String_val(v_s);
  intnat pos = Long_val(v_pos);
  intnat len = Long_val(v_len);
  blake3_mini_check_range(pos, len, caml_string_length(v_s));
  blake3_hasher_update(hasher, s + pos, len);

  CAMLreturn(Val_unit);
}

CAMLprim value blake3_mini_feed_bigstring_unlock(value v_t, value v_s,
                                                 value v_pos, value v_len) {
  CAMLparam4(v_t, v_s, v_pos, v_len);

  blake3_hasher *hasher = Blake3_val(v_t);
  intnat pos = Long_val(v_pos);
  intnat len = Long_val(v_len);
  blake3_mini_check_range(pos, len, Caml_ba_array_val(v_s)->dim[0]);
  char *s = Caml_ba_data_val(v_s);
  caml_register_global_root(&v_s);
  caml_release_runtime_system();
  blake3_hasher_update(hasher, s + pos, len);
  caml_acquire_runtime_system();
  caml_remove_global_root(&v_s);

  CAMLreturn(Val_unit);
}
