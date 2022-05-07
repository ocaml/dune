/* File curses_stubs.c -- stub code for curses */
#include <curses.h>
#include <caml/mlvalues.h>
#include <caml/memory.h>
#include <caml/alloc.h>
#include <caml/custom.h>

value test_print(value text) {
  CAMLparam1 (text);
  printf("%s\n", String_val(text));
  CAMLreturn (Val_unit);
}
