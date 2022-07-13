#define CAML_NAME_SPACE
#include "caml/mlvalues.h"

CAMLprim value caml_b_or_n2(value unit)          /* ML */
{
  return Val_int(42);
}
