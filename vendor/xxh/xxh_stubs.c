#include <caml/alloc.h>
#include <caml/custom.h>
#include <caml/fail.h>
#include <caml/memory.h>
#include <caml/mlvalues.h>
#include <caml/threads.h>
#include <caml/unixsupport.h>

#include <unistd.h>

#include "xxhash.h"

static inline value alloc_128(XXH128_hash_t hash) {
  value v_ret = caml_alloc_string(16);
  uint64_t *ret = (uint64_t *)&Byte(v_ret, 0);
  uint64_t low = hash.low64;
  uint64_t high = hash.high64;
  ret[0] = hash.low64;
  ret[1] = hash.high64;
  return v_ret;
}

CAMLprim value xxh3_128_string(value v_s) {
  CAMLparam1(v_s);
  CAMLlocal1(v_ret);
  int len = caml_string_length(v_s);
  XXH128_hash_t hash = XXH3_128bits(&Byte(v_s, 0), len);
  v_ret = alloc_128(hash);
  CAMLreturn(v_ret);
}

CAMLprim value xxh3_64_string(value v_s) {
  CAMLparam1(v_s);
  CAMLlocal1(v_ret);
  int len = caml_string_length(v_s);
  uint64_t hash = XXH3_64bits(&Byte(v_s, 0), len);
  v_ret = caml_copy_int64(hash);
  CAMLreturn(v_ret);
}

CAMLprim value xxh_128_fd(value v_fd) {
  CAMLparam1(v_fd);
  CAMLlocal1(v_ret);
  int fd = Int_val(v_fd);
  caml_release_runtime_system();
  XXH3_state_t *state = XXH3_createState();
  if (state == NULL)
    caml_raise_out_of_memory();
  if (XXH3_128bits_reset(state) == XXH_ERROR) {
    caml_failwith("xxh_128_fd: failed to reset");
  }
  char buffer[UNIX_BUFFER_SIZE];
  size_t count;
  while ((count = read(fd, buffer, sizeof(buffer))) != 0) {
    XXH3_128bits_update(state, buffer, count);
  }
  XXH128_hash_t hash = XXH3_128bits_digest(state);
  XXH3_freeState(state);
  caml_acquire_runtime_system();
  v_ret = alloc_128(hash);
  CAMLreturn(v_ret);
}

#define Stream_val(v) (*(XXH3_state_t **)Data_custom_val(v))

static void xxh_stream_finalize(value v_state) {
  XXH3_state_t *state = Stream_val(v_state);
  XXH3_freeState(state);
}

static struct custom_operations xxh_stream_t_ops = {
    "xxh.stream",
    xxh_stream_finalize,
    custom_compare_default,
    custom_hash_default,
    custom_serialize_default,
    custom_deserialize_default,
    custom_compare_ext_default,
    custom_fixed_length_default};

CAMLprim value xxh_create(value v_unit) {
  CAMLparam1(v_unit);
  XXH3_state_t *state = XXH3_createState();
  if (XXH3_128bits_reset(state) == XXH_ERROR) {
    caml_failwith("xxh_128bits_reset: failed to reset");
  }
  value v_state =
      caml_alloc_custom(&xxh_stream_t_ops, sizeof(XXH3_state_t *), 0, 1);
  Stream_val(v_state) = state;
  CAMLreturn(v_state);
}

CAMLprim value xxh_feed_bytes(value v_state, value v_bytes, value v_pos,
                              value v_len) {
  CAMLparam4(v_state, v_bytes, v_pos, v_len);
  XXH3_state_t *state = Stream_val(v_state);
  XXH3_128bits_update(state, &Byte(v_bytes, Int_val(v_pos)), Int_val(v_len));
  CAMLreturn(Val_unit);
}

CAMLprim value xxh_128bits(value v_state) {
  CAMLparam1(v_state);
  CAMLlocal1(v_ret);
  XXH3_state_t *state = Stream_val(v_state);
  XXH128_hash_t hash = XXH3_128bits_digest(state);
  v_ret = alloc_128(hash);
  CAMLreturn(v_ret);
}

CAMLprim value xxh_reset(value v_state) {
  CAMLparam1(v_state);
  XXH3_state_t *state = Stream_val(v_state);
  if (XXH3_128bits_reset(state) == XXH_ERROR) {
    caml_failwith("xxh_128bits_reset: failed to reset");
  }
  CAMLreturn(Val_unit);
}
