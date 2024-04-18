#define _GNU_SOURCE
#include <stdbool.h>
#include <fcntl.h>
#include <math.h>
#include <sys/stat.h>
#include <caml/mlvalues.h>
#include <caml/memory.h>
#include <caml/alloc.h>

#ifndef Val_none
#define Val_none Val_int(0)
#endif /* Val_none */

#ifndef Tag_some
#define Tag_some 0
static CAMLprim value caml_alloc_some_(value v) {
	CAMLparam1(v);
	CAMLlocal1(some);

	some = caml_alloc_small(1, Tag_some);
	Store_field(some, 0, v);
	CAMLreturn(some);
}
#endif /* Tag_some */


/* from stat_unix.c * */
static double stat_timestamp(time_t sec, long nsec)
{
  /* The conversion of sec to FP is exact for the foreseeable future.
     (It starts rounding when sec > 2^53, i.e. in 285 million years.) */
  double s = (double) sec;
  /* The conversion of nsec to fraction of seconds can round.
     Still, we have 0 <= n < 1.0. */
  double n = (double) nsec / 1e9;
  /* The sum s + n can round up, hence s <= t + <= s + 1.0 */
  double t = s + n;
  /* Detect the "round up to s + 1" case and decrease t so that
     its integer part is s. */
  if (t == s + 1.0) t = nextafter(t, s);
  return t;
}

static int dune_creation_time__statx(bool follow, const char* pathname, double* btime) {
#ifdef HAVE_STATX
    int flags = 0;
    if (!follow) {
      flags = AT_SYMLINK_NOFOLLOW;
    }
    struct statx stx = { 0, };
    int rc = statx(AT_FDCWD, pathname, flags, STATX_BTIME, &stx);

    if (rc == 0) {
        if (stx.stx_btime.tv_sec != 0) {
	     *btime = stat_timestamp(stx.stx_btime.tv_sec, stx.stx_btime.tv_nsec);
             return 0;
        }
    }
#endif /* HAVE_STATX */
    return -1;
}

CAMLprim value dune_creation_time__generic(bool follow, value path) {
    CAMLparam1(path);
    CAMLlocal1(caml_ts);

    double ts;
    const char* pathname = String_val(path);

    if (dune_creation_time__statx(follow, pathname, &ts) == 0) {
	caml_ts = caml_copy_double(ts);
	CAMLreturn (caml_alloc_some(caml_ts));
    } else {
	CAMLreturn (Val_none);
    }
}

CAMLprim value dune_creation_time__lstat(value path) {
    CAMLparam1(path);
    CAMLreturn (dune_creation_time__generic(false, path));
}

CAMLprim value dune_creation_time__stat(value path) {
    CAMLparam1(path);
    CAMLreturn (dune_creation_time__generic(true, path));
}
