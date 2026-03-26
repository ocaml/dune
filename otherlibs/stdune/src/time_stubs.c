#include <caml/mlvalues.h>
#include <caml/alloc.h>

#include "dune_clock.h"

CAMLprim value dune_clock_gettime_realtime(value v_unit) {
  (void)v_unit;
  return Val_long(dune_clock_gettime_ns());
}
