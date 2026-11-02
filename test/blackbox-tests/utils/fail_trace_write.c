#define _GNU_SOURCE
#include <dlfcn.h>
#include <errno.h>
#include <limits.h>
#include <stdio.h>
#include <string.h>
#include <sys/syscall.h>
#include <unistd.h>

ssize_t write(int fd, const void *buf, size_t len) {
  static ssize_t (*real_write)(int, const void *, size_t);
  if (!real_write) real_write = dlsym(RTLD_NEXT, "write");

  if (syscall(SYS_gettid) != getpid()) {
    char link[64], path[PATH_MAX];
    snprintf(link, sizeof(link), "/proc/self/fd/%d", fd);
    ssize_t n = readlink(link, path, sizeof(path) - 1);
    if (n >= 0) {
      path[n] = '\0';
      if (strstr(path, "trace.csexp")) {
        errno = ENOSPC;
        return -1;
      }
    }
  }
  return real_write(fd, buf, len);
}
