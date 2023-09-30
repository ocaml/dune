#include <caml/memory.h>
#include <caml/mlvalues.h>

CAMLprim value stdune_is_darwin(value v_unit) {
  CAMLparam1(v_unit);
#if defined(__APPLE__)
  CAMLreturn(Val_true);
#else
  CAMLreturn(Val_false);
#endif
}

CAMLprim value stdune_is_freebsd(value v_unit) {
  CAMLparam1(v_unit);
#if defined(__FreeBSD__)
  CAMLreturn(Val_true);
#else
  CAMLreturn(Val_false);
#endif
}

CAMLprim value stdune_is_openbsd(value v_unit) {
  CAMLparam1(v_unit);
#if defined(__OpenBSD__)
  CAMLreturn(Val_true);
#else
  CAMLreturn(Val_false);
#endif
}

CAMLprim value stdune_is_netbsd(value v_unit) {
  CAMLparam1(v_unit);
#if defined(__NetBSD__)
  CAMLreturn(Val_true);
#else
  CAMLreturn(Val_false);
#endif
}

CAMLprim value stdune_is_haiku(value v_unit) {
  CAMLparam1(v_unit);
#if defined(__HAIKU__)
  CAMLreturn(Val_true);
#else
  CAMLreturn(Val_false);
#endif
}