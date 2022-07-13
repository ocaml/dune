#define CAML_NAME_SPACE
#include "caml/mlvalues.h"

CAMLprim value caml_42(value unit)          /* ML */
{
  return Val_int(42);
}
