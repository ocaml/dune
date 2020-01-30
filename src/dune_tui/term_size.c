#include <caml/mlvalues.h>
#include <caml/alloc.h>
#include <caml/unixsupport.h>

#if defined(_WIN32) || defined(_WIN64)

CAMLprim value dune_tui_get_size_from_fd(value fd)
{
  value result = caml_alloc_tuple(2);
  Field(result, 0) = Val_int(25);
  Field(result, 1) = Val_int(80);
  return result;
}

#else

#include <sys/ioctl.h>
#include <termios.h>

CAMLprim value dune_tui_get_size_from_fd(value fd)
{
  struct winsize size;

  if (ioctl(Int_val(fd), TIOCGWINSZ, &size) < 0)
    uerror("ioctl", Nothing);

  value result = caml_alloc_tuple(2);
  Field(result, 0) = Val_int(size.ws_row);
  Field(result, 1) = Val_int(size.ws_col);
  return result;
}

#endif
