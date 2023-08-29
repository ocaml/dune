#include <caml/memory.h>
#include <caml/mlvalues.h>

// MacOS
CAMLprim value stdune_is_darwin(value v_unit) {
  CAMLparam1(v_unit);
#if defined(__APPLE__)
  CAMLreturn(Val_true);
#else
  CAMLreturn(Val_false);
#endif
}

// FreeBSD
CAMLprim value stdune_is_freebsd(value v_unit) {
  CAMLparam1(v_unit);
#if defined(__FreeBSD__)
  CAMLreturn(Val_true);
#else
  CAMLreturn(Val_false);
#endif
}

// OpenBSD
CAMLprim value stdune_is_openbsd(value v_unit) {
  CAMLparam1(v_unit);
#if defined(__OpenBSD__)
  CAMLreturn(Val_true);
#else
  CAMLreturn(Val_false);
#endif
}

// NetBSD
CAMLprim value stdune_is_netbsd(value v_unit) {
  CAMLparam1(v_unit);
#if defined(__NetBSD__)
  CAMLreturn(Val_true);
#else
  CAMLreturn(Val_false);
#endif
}

// DragonFly BSD
CAMLprim value stdune_is_dragonfly(value v_unit) {
  CAMLparam1(v_unit);
#if defined(__DragonFly__)
  CAMLreturn(Val_true);
#else
  CAMLreturn(Val_false);
#endif
}

// Haiku
CAMLprim value stdune_is_haiku(value v_unit) {
  CAMLparam1(v_unit);
#if defined(__HAIKU__)
  CAMLreturn(Val_true);
#else
  CAMLreturn(Val_false);
#endif
}

// Serenity
CAMLprim value stdune_is_serenity(value v_unit) {
  CAMLparam1(v_unit);
#if defined(__serenity__)
  CAMLreturn(Val_true);
#else
  CAMLreturn(Val_false);
#endif
}

// Solaris
CAMLprim value stdune_is_solaris(value v_unit) {
  CAMLparam1(v_unit);
#if defined(__sun)
  CAMLreturn(Val_true);
#else
  CAMLreturn(Val_false);
#endif
}
