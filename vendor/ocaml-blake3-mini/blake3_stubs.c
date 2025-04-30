#include <errno.h>

#include <caml/alloc.h>
#include <caml/bigarray.h>
#include <caml/custom.h>
#include <caml/fail.h>
#include <caml/memory.h>
#include <caml/mlvalues.h>
#include <caml/threads.h>
#include <caml/unixsupport.h>

#include "blake3.h"

#define Blake3_val(v) (*(blake3_hasher **)Data_custom_val(v))

static inline value alloc_hash(blake3_hasher *hasher, int len) {
  value v_ret = caml_alloc_string(len);
  const char *ret = String_val(v_ret);
  blake3_hasher_finalize(hasher, (uint8_t *) ret, len);
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
  v_ret = alloc_hash(&hasher, 16);
  CAMLreturn(v_ret);
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
  v_ret = alloc_hash(hasher, 16);

  CAMLreturn(v_ret);
}

CAMLprim value blake3_mini_feed_string(value v_t, value v_s, value v_pos,
                                       value v_len) {
  CAMLparam4(v_t, v_s, v_pos, v_len);

  blake3_hasher *hasher = Blake3_val(v_t);
  const char *s = String_val(v_s);
  size_t pos = Long_val(v_pos);
  size_t len = Long_val(v_len);
  blake3_hasher_update(hasher, s + pos, len);

  CAMLreturn(Val_unit);
}

CAMLprim value blake3_mini_feed_bigstring_unlock(value v_t, value v_s,
                                                 value v_pos, value v_len) {
  CAMLparam4(v_t, v_s, v_pos, v_len);

  blake3_hasher *hasher = Blake3_val(v_t);
  size_t pos = Long_val(v_pos);
  size_t len = Long_val(v_len);
  char *s = Caml_ba_data_val(v_s);
  caml_register_global_root(&v_s);
  caml_release_runtime_system();
  blake3_hasher_update(hasher, s + pos, len);
  caml_acquire_runtime_system();
  caml_remove_global_root(&v_s);

  CAMLreturn(Val_unit);
}
