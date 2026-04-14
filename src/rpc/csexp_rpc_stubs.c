#include <caml/alloc.h>
#include <caml/callback.h>
#include <caml/custom.h>
#include <caml/fail.h>
#include <caml/memory.h>
#include <caml/mlvalues.h>
#include <caml/signals.h>
#include <caml/unixsupport.h>

#if defined(__APPLE__)

#include <fcntl.h>
#include <sys/socket.h>
#include <sys/syscall.h>
#include <sys/types.h>
#include <unistd.h>

CAMLprim value dune_pthread_chdir_is_osx(value unit) {
  CAMLparam1(unit);
  CAMLreturn(Val_true);
}

#ifndef SYS___pthread_chdir
#define SYS___pthread_chdir 348
#endif

static int __pthread_chdir(const char *path) {
#pragma clang diagnostic push
#pragma clang diagnostic ignored "-Wdeprecated"
  return syscall(SYS___pthread_chdir, path);
#pragma clang diagnostic pop
}

CAMLprim value dune_pthread_chdir(value dir) {
  CAMLparam1(dir);
  if (__pthread_chdir(String_val(dir))) {
    uerror("__pthread_chdir", dir);
  }
  CAMLreturn(Val_unit);
}

CAMLprim value dune_set_nosigpipe(value v_socket) {
  CAMLparam1(v_socket);
  int socket = Int_val(v_socket);
  int opt = 1;
  int ret = setsockopt(socket, SOL_SOCKET, SO_NOSIGPIPE, &opt, sizeof(int));
  if (ret < 0) {
    uerror("setsockopt", Nothing);
  }
  CAMLreturn(Val_unit);
}

#else

CAMLprim value dune_set_nosigpipe(value v_socket) {
  caml_invalid_argument("only implemented on macos");
}

CAMLprim value dune_pthread_chdir_is_osx(value unit) {
  CAMLparam1(unit);
  CAMLreturn(Val_false);
}

CAMLprim value dune_pthread_chdir(value unit) {
  CAMLparam1(unit);
  caml_invalid_argument("__pthread_chdir not implemented");
  CAMLreturn(Val_unit);
}

#endif

#if __linux__

#include <caml/threads.h>
#include <string.h>
#include <sys/socket.h>
#include <sys/types.h>

CAMLprim value dune_send(value v_fd, value v_bytes, value v_pos, value v_len) {
  CAMLparam4(v_fd, v_bytes, v_pos, v_len);
  int len = Long_val(v_len);
  if (len > UNIX_BUFFER_SIZE) {
    len = UNIX_BUFFER_SIZE;
  }
  int pos = Long_val(v_pos);
  int fd = Int_val(v_fd);
  char iobuf[UNIX_BUFFER_SIZE];
  memmove(iobuf, &Byte(v_bytes, pos), len);
  caml_release_runtime_system();
  int ret = send(fd, iobuf, len, MSG_NOSIGNAL);
  caml_acquire_runtime_system();
  if (ret == -1) {
    uerror("send", Nothing);
  };
  CAMLreturn(Val_int(ret));
}
#else
CAMLprim value dune_send(value v_fd, value v_bytes, value v_pos, value v_len) {
  (void)v_fd;
  (void)v_bytes;
  (void)v_pos;
  (void)v_len;
  caml_invalid_argument("sendmsg without sigpipe only available on linux");
}
#endif
