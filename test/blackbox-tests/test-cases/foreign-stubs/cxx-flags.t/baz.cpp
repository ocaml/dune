#include <caml/mlvalues.h>
#include <caml/io.h>
#include <iostream>

extern "C" value baz(value unit) { return Val_int(2046); }

extern "C" void hello_world_baz ();

void hello_world_baz ()
{
  std::cout << "Hello World Baz!\n";
}
