#include <stdio.h>
#include <caml/mlvalues.h>

void foo(value s)
{
  puts(String_val(s));
}
