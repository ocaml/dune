#include <caml/mlvalues.h>
#include <caml/config.h>

/* [Max_young_wosize] is OCaml's minor-heap allocation limit, in words. */
CAMLprim value dune_memo_max_young_wosize(value unit)
{
  (void)unit;
  return Val_long(Max_young_wosize);
}
