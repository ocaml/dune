#include <signal.h>
#include <caml/mlvalues.h>

CAMLprim value dune_winch_number (value unit) {
  return Val_int (SIGWINCH);
}
