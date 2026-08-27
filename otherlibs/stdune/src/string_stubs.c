#include <caml/mlvalues.h>

#include <stddef.h>
#include <string.h>

CAMLprim value dune_string_index_from(value v_string, value v_position, value v_char)
{
  const unsigned char *string = (const unsigned char *)String_val(v_string);
  size_t length = caml_string_length(v_string);
  size_t position = Long_val(v_position);
  const unsigned char *result =
    (const unsigned char *)memchr(string + position, Int_val(v_char), length - position);
  return Val_long(result == NULL ? -1 : result - string);
}
