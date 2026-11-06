#define _GNU_SOURCE
#include <dlfcn.h>
#include <errno.h>
#include <limits.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>

ssize_t write(int fd, const void *buf, size_t len) {
  static ssize_t (*real_write)(int, const void *, size_t);
  if (!real_write) real_write = dlsym(RTLD_NEXT, "write");

  const char *marker = getenv("DUNE_TRACE_FAIL_AFTER");
  if (marker && access(marker, F_OK) != 0) return real_write(fd, buf, len);

  /* Match the event header, not a process event's exit-status argument. */
  const char *console = getenv("DUNE_TRACE_FAIL_CONSOLE");
  const char *console_name = "(4:name7:console)";
  const char *cleanup = getenv("DUNE_TRACE_FAIL_CLEANUP");
  const char *cleanup_signal = "(6:signal4:USR1)";
  const char *event = console
      ? "(6:thread12:spawn_thread"
      : getenv("DUNE_TRACE_FAIL_AT_EXIT")
          ? "(6:config4:exit"
          : "(7:process15:signal_received";
  if (memmem(buf, len, event, strlen(event)) &&
      (!console || memmem(buf, len, console_name, strlen(console_name))) &&
      (!cleanup || memmem(buf, len, cleanup_signal, strlen(cleanup_signal)))) {
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
