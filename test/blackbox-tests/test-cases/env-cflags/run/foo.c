#include <stdio.h>

int main(int argc, char ** argv){
#ifdef TEST_C
  printf("TEST_C defined.\n");
  return 0;
#else
  printf("TEST_C not defined.\n");
  return 1;
#endif
}
