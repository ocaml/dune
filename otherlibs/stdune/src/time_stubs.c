#if defined(__APPLE__)
#ifndef _DARWIN_C_SOURCE
#define _DARWIN_C_SOURCE 1
#endif
#endif

#if !defined(_WIN32) \
    && !defined(__APPLE__) \
    && !defined(__FreeBSD__) \
    && !defined(__NetBSD__) \
    && !defined(__OpenBSD__) \
    && !defined(__DragonFly__)
#ifndef _POSIX_C_SOURCE
#define _POSIX_C_SOURCE 200809L
#endif
#endif

#include <caml/memory.h>
#include <caml/mlvalues.h>
#include <caml/alloc.h>
#include <caml/osdeps.h>
#include <caml/threads.h>
#include <caml/unixsupport.h>

#include <sys/stat.h>

#include "dune_clock.h"

CAMLprim value dune_clock_gettime_realtime(value v_unit) {
  (void)v_unit;
  return Val_long(dune_clock_gettime_ns());
}

#ifdef _WIN32
typedef struct _stati64 dune_time_stat_buf;
#else
typedef struct stat dune_time_stat_buf;
#endif

static int64_t dune_time_stat_mtime_ns(const dune_time_stat_buf *stat)
{
#ifdef _WIN32
  return ((int64_t) stat->st_mtime) * 1000000000LL;
#elif defined(__APPLE__) || defined(__FreeBSD__) || defined(__NetBSD__) \
   || defined(__OpenBSD__) || defined(__DragonFly__)
  return ((int64_t) stat->st_mtimespec.tv_sec * 1000000000LL)
         + (int64_t) stat->st_mtimespec.tv_nsec;
#elif defined(__HAIKU__)
  return ((int64_t) stat->st_mtime) * 1000000000LL;
#else
  return ((int64_t) stat->st_mtim.tv_sec * 1000000000LL)
         + (int64_t) stat->st_mtim.tv_nsec;
#endif
}

static value dune_time_stat_file_kind(int mode)
{
#ifdef _WIN32
  if ((mode & _S_IFMT) == _S_IFDIR) return Val_int(1);
  if ((mode & _S_IFMT) == _S_IFCHR) return Val_int(2);
  if ((mode & _S_IFMT) == _S_IFIFO) return Val_int(5);
  return Val_int(0);
#else
  if (S_ISDIR(mode)) return Val_int(1);
  if (S_ISCHR(mode)) return Val_int(2);
#ifdef S_ISBLK
  if (S_ISBLK(mode)) return Val_int(3);
#endif
#ifdef S_ISLNK
  if (S_ISLNK(mode)) return Val_int(4);
#endif
  if (S_ISFIFO(mode)) return Val_int(5);
#ifdef S_ISSOCK
  if (S_ISSOCK(mode)) return Val_int(6);
#endif
  return Val_int(0);
#endif
}

static value dune_time_alloc_stat(const dune_time_stat_buf *stat)
{
  CAMLparam0();
  CAMLlocal1(v_stat);
  v_stat = caml_alloc_small(6, 0);
  Field(v_stat, 0) = Val_long(dune_time_stat_mtime_ns(stat));
  Field(v_stat, 1) = Val_long(stat->st_size);
  Field(v_stat, 2) = Val_long(stat->st_mode & 07777);
  Field(v_stat, 3) = dune_time_stat_file_kind(stat->st_mode);
  Field(v_stat, 4) = Val_long(stat->st_dev);
  Field(v_stat, 5) = Val_long(stat->st_ino);
  CAMLreturn(v_stat);
}

CAMLprim value dune_stat(value v_path) {
  CAMLparam1(v_path);
  caml_unix_check_path(v_path, "stat");
  char_os *path = caml_stat_strdup_to_os(String_val(v_path));
  dune_time_stat_buf statbuf;
  int rc;
  caml_release_runtime_system();
  rc = stat_os(path, &statbuf);
  caml_acquire_runtime_system();
  caml_stat_free(path);
  if (rc == -1) {
    uerror("stat", v_path);
  }
  CAMLreturn(dune_time_alloc_stat(&statbuf));
}
