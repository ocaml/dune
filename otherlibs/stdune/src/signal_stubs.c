#include <caml/mlvalues.h>

CAMLextern int caml_convert_signal_number(int);

#ifdef _WIN32
#include <caml/fail.h>
#else
#include <signal.h>
#include <sys/ioctl.h>
#endif

CAMLprim value stdune_winch_number(value vunit) {
  (void)vunit;
#ifdef _WIN32
  return Val_int(0);
#else
  return Val_int(SIGWINCH);
#endif
}

CAMLprim value stdune_system_signal_number(value signal) {
  return Val_int(caml_convert_signal_number(Int_val(signal)));
}
