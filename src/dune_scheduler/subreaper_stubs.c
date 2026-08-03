#include <caml/mlvalues.h>

#if defined(__linux__)

#include <caml/unixsupport.h>
#include <errno.h>
#include <sys/prctl.h>

CAMLprim value dune_scheduler_set_child_subreaper(value v_unit) {
  (void)v_unit;
  if (prctl(PR_SET_CHILD_SUBREAPER, 1, 0, 0, 0) == -1) {
    if (errno == EINVAL) return Val_false;
    uerror("prctl", Nothing);
  }
  return Val_true;
}

#else

CAMLprim value dune_scheduler_set_child_subreaper(value v_unit) {
  (void)v_unit;
  return Val_false;
}

#endif
