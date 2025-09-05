#include <caml/mlvalues.h>
#include <iostream>

extern "C" value bazexe(value unit) { return Val_int(4096); }

extern "C" void hello_world_bazexe ();

void hello_world_bazexe ()
{
  std::cout << "Hello World Bazexe!";
}
