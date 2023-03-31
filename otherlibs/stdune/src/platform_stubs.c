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
