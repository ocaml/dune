#include <caml/mlvalues.h>

int get_int(void);

value get_int_ocaml(value x) {
  return Val_int(get_int());
}
