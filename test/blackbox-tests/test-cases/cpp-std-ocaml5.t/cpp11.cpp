#include <iostream>
#include <cstring>
#include <caml/misc.h>
#include <caml/mlvalues.h>

// strdup is not part of the C standard library but is part of POSIX.
// By default GCC and Clang both use the GNU variant of the C standard,
// so we are using strdup here to ensure users have access to it by default.
// If -std=c++11 is used instead of -std=gnu++11 this would fail to compile
// on non-POSIX platforms such as Cygwin.
extern "C" CAMLprim value cpp11(value _unit) {
  std::cout << strdup("Hi from C++11") << std::endl;
  return Val_unit;
}
