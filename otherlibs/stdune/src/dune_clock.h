// Cross-platform realtime clock with legacy fallbacks
//
// Provides dune_clock_gettime_ns() returning nanoseconds since Unix epoch.
//
// Platform support:
//  >= Windows 8    GetSystemTimePreciseAsFileTime
//   < Windows 8    GetSystemTimeAsFileTime
//  >= macOS 10.12  clock_gettime
//   < macOS 10.12  gettimeofday
//     Other Unix   clock_gettime

#ifndef DUNE_CLOCK_H
#define DUNE_CLOCK_H

#include <stdint.h>

#ifdef _WIN32
#include <windows.h>

// Windows epoch starts 1601-01-01, Unix epoch starts 1970-01-01
// Difference is 11644473600 seconds
#define SEC_TO_UNIX_EPOCH 11644473600LL

static inline int64_t dune_clock_gettime_ns(void) {
  FILETIME ft;
  static VOID (WINAPI *GetSystemTime)(LPFILETIME) = NULL;

  if (GetSystemTime == NULL) {
    HMODULE h = GetModuleHandleA("kernel32.dll");
    if (h) {
      GetSystemTime = (VOID (WINAPI *)(LPFILETIME))GetProcAddress(h, "GetSystemTimePreciseAsFileTime");
    }
    if (GetSystemTime == NULL) {
      // Fallback for Windows < 8
      GetSystemTime = GetSystemTimeAsFileTime;
    }
  }

  GetSystemTime(&ft);
  ULARGE_INTEGER li;
  li.LowPart = ft.dwLowDateTime;
  li.HighPart = ft.dwHighDateTime;

 // Convert from 100ns intervals since Windows epoch to ns since Unix epoch
  return (li.QuadPart - SEC_TO_UNIX_EPOCH * 10000000LL) * 100;
}

#else // _WIN32

#include <time.h>

#if defined(__APPLE__)
#include <sys/time.h>
#include <AvailabilityMacros.h>
#endif

static inline int64_t dune_clock_gettime_ns(void) {
#if defined(__APPLE__) && MAC_OS_X_VERSION_MIN_REQUIRED < 101200
  // macOS < 10.12 doesn't have clock_gettime, use gettimeofday
  struct timeval tv;
  gettimeofday(&tv, NULL);
  return ((int64_t)tv.tv_sec * 1000000000LL) + ((int64_t)tv.tv_usec * 1000LL);
#else
  struct timespec tp;
  clock_gettime(CLOCK_REALTIME, &tp);
  return ((int64_t)tp.tv_sec * 1000000000LL) + (int64_t)tp.tv_nsec;
#endif
}

#endif // _WIN32
#endif // DUNE_CLOCK_H
