#include <caml/mlvalues.h>
#include <stdio.h>

value ml_hello(value vunit)
{
  printf("Hello!\n");
  fflush(stdout);
  return Val_unit;
}
