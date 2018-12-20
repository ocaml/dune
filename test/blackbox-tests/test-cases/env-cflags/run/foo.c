#include <stdio.h>

int main(int argc, char ** argv){
#ifdef TEST
  printf("DTEST_C defined.\n");
  return 0;
#else
  printf("DTEST_C not defined.\n");
  return 1;
#endif
}
