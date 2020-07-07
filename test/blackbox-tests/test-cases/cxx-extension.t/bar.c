#include <stdio.h>
#include "foo.cxx"

void foo () {
  int n = cppfoo();
  printf("n = %d\n", n);
}
