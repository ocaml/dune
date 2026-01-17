#include <caml/mlvalues.h>
#include <caml/alloc.h>
#include <stdint.h>

#ifdef _WIN32
#include <windows.h>

// Windows epoch starts 1601-01-01, Unix epoch starts 1970-01-01
// Difference is 11644473600 seconds
#define SEC_TO_UNIX_EPOCH 11644473600LL

CAMLprim value dune_clock_gettime_realtime(value v_unit) {
  (void)v_unit;
  FILETIME ft;
  GetSystemTimePreciseAsFileTime(&ft);

  ULARGE_INTEGER li;
  li.LowPart = ft.dwLowDateTime;
  li.HighPart = ft.dwHighDateTime;

  // Convert from 100ns intervals since Windows epoch to ns since Unix epoch
  int64_t ns = (li.QuadPart - SEC_TO_UNIX_EPOCH * 10000000LL) * 100;
  return Val_long(ns);
}

#else  // Unix-like systems

#include <time.h>

CAMLprim value dune_clock_gettime_realtime(value v_unit) {
  (void)v_unit;
  struct timespec tp;
  clock_gettime(CLOCK_REALTIME, &tp);
  int64_t ns = ((int64_t)tp.tv_sec * 1000000000LL) + (int64_t)tp.tv_nsec;
  return Val_long(ns);
}

#endif
