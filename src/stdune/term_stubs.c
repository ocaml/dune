#include <caml/mlvalues.h>
#include <caml/alloc.h>

#if defined(_WIN32) || defined(_WIN64)

CAMLprim value stdune_get_sigwinch()
{
  return Val_int(0);
}

#else

#include <signal.h>

CAMLprim value stdune_get_sigwinch()
{
#ifdef SIGWINCH
  value result = caml_alloc_tuple(1);
  Field(result, 0) = Val_int(SIGWINCH);
  return result;
#else
  return Val_int(0);
#endif
}

#endif
