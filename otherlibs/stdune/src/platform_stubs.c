#if defined(__linux__)
#define _GNU_SOURCE
#include <sched.h>
#endif

#include <ctype.h>
#include <errno.h>
#include <stdlib.h>

#if defined(_WIN32)
#include <windows.h>
#else
#include <unistd.h>
#endif

#include <caml/memory.h>
#include <caml/mlvalues.h>

CAMLprim value stdune_is_darwin(value v_unit) {
  CAMLparam1(v_unit);
#if defined(__APPLE__)
  CAMLreturn(Val_true);
#else
  CAMLreturn(Val_false);
#endif
}

CAMLprim value stdune_is_freebsd(value v_unit) {
  CAMLparam1(v_unit);
#if defined(__FreeBSD__)
  CAMLreturn(Val_true);
#else
  CAMLreturn(Val_false);
#endif
}

CAMLprim value stdune_is_openbsd(value v_unit) {
  CAMLparam1(v_unit);
#if defined(__OpenBSD__)
  CAMLreturn(Val_true);
#else
  CAMLreturn(Val_false);
#endif
}

CAMLprim value stdune_is_netbsd(value v_unit) {
  CAMLparam1(v_unit);
#if defined(__NetBSD__)
  CAMLreturn(Val_true);
#else
  CAMLreturn(Val_false);
#endif
}

CAMLprim value stdune_is_dragonfly(value v_unit) {
  CAMLparam1(v_unit);
#if defined(__DragonFly__)
  CAMLreturn(Val_true);
#else
  CAMLreturn(Val_false);
#endif
}

CAMLprim value stdune_is_haiku(value v_unit) {
  CAMLparam1(v_unit);
#if defined(__HAIKU__)
  CAMLreturn(Val_true);
#else
  CAMLreturn(Val_false);
#endif
}

static long positive_env_limit(const char *name) {
  const char *value = getenv(name);
  const char *start;
  char *end;
  long limit;
  if (value == NULL) return 0;
  start = value;
  while (isspace((unsigned char)*start)) start++;
  if (*start < '0' || *start > '9') return 0;
  errno = 0;
  limit = strtol(start, &end, 10);
  if (errno == ERANGE || limit < 1 || limit > Max_long) return 0;
  while (isspace((unsigned char)*end)) end++;
  return *end == '\0' || *end == ',' ? limit : 0;
}

CAMLprim value stdune_cpu_count(value v_unit) {
  long count = 0;
  long limit;
  (void)v_unit;

#if defined(__linux__)
  long configured = sysconf(_SC_NPROCESSORS_CONF);
  if (configured > 0) {
    size_t affinity_size = CPU_ALLOC_SIZE(configured);
    cpu_set_t *affinity = CPU_ALLOC(configured);
    if (affinity != NULL) {
      CPU_ZERO_S(affinity_size, affinity);
      if (sched_getaffinity(0, affinity_size, affinity) == 0)
        count = CPU_COUNT_S(affinity_size, affinity);
      CPU_FREE(affinity);
    }
  }
#endif

#if defined(_WIN32)
  if (count < 1) count = GetActiveProcessorCount(ALL_PROCESSOR_GROUPS);
#elif defined(_SC_NPROCESSORS_ONLN)
  if (count < 1) count = sysconf(_SC_NPROCESSORS_ONLN);
#endif

  if (count < 1) return Val_long(0);
  limit = positive_env_limit("OMP_NUM_THREADS");
  if (limit > 0 && limit < count) count = limit;
  limit = positive_env_limit("OMP_THREAD_LIMIT");
  if (limit > 0 && limit < count) count = limit;
  return Val_long(count);
}