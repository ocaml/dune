#include <caml/alloc.h>

value hello_in_utop(value v_unit)
{
  return caml_copy_string("hello in utop");
}
