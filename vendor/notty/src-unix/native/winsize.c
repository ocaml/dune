#include <caml/mlvalues.h>

#ifdef _WIN32
#include <caml/fail.h>
#else
#include <sys/ioctl.h>
#include <signal.h>
#endif

CAMLprim value caml_notty_winsize (value vfd) {
#ifdef _WIN32
  (void) vfd;
  caml_failwith("not implemented on windows");
#else
  int fd = Int_val (vfd);
  struct winsize w;
  if (ioctl (fd, TIOCGWINSZ, &w) >= 0)
    return Val_int ((w.ws_col << 16) + ((w.ws_row & 0x7fff) << 1));
  return Val_int (0);
#endif
}

CAMLprim value caml_notty_winch_number (value vunit) {
  (void) vunit;
#ifdef _WIN32
  return Val_int (0);
#else
  return Val_int (SIGWINCH);
#endif
}
