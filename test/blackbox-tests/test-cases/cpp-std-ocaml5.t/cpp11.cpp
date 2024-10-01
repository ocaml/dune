#include <caml/misc.h>
#include <caml/mlvalues.h>
#include <iostream>

extern "C" CAMLprim value cpp11(value _unit) {
  std::cout << "Hi from C++11" << std::endl;
  return Val_unit;
}
