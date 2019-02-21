#include <stdio.h>

int main(int argc, char ** argv){
#ifdef TEST_CPP
  printf("TEST_CPP defined.\n");
  return 0;
#else
  printf("TEST_CPP not defined.\n");
  return 1;
#endif
}
