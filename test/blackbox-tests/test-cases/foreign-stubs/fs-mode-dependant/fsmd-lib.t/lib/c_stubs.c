#include "caml/mlvalues.h"

value caml_b_or_n(value unit)
{
#ifdef NATIVE_CODE
  return Val_int(1);
#else
  return Val_int(0);
#endif
}
