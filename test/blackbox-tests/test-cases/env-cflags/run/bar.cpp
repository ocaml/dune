#include <stdio.h>

int main(int argc, char ** argv){
#ifdef TEST
  printf("DTEST_CPP defined.\n");
  return 0;
#else
  printf("DTEST_CPP not defined.\n");
  return 1;
#endif
}
