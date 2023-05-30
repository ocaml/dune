#include <caml/mlvalues.h>

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
