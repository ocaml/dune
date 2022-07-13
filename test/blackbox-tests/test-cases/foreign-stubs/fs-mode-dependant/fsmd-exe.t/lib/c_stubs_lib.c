#define CAML_NAME_SPACE
#include "caml/mlvalues.h"

#ifdef NATIVE_CODE
CAMLprim value caml_b_or_n(value unit)          /* ML */
{
  return Val_int(1);
}
#else
CAMLprim value caml_b_or_n(value unit)          /* ML */
{
  return Val_int(0);
}
#endif
