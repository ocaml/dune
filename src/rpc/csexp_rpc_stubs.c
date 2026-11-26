#include <caml/alloc.h>
#include <caml/callback.h>
#include <caml/custom.h>
#include <caml/fail.h>
#include <caml/memory.h>
#include <caml/mlvalues.h>
#include <caml/signals.h>
#include <caml/unixsupport.h>

#ifdef _WIN32
#include <windows.h>
#endif

CAMLprim value dune_rpc_registry_windows_process_id(value unit) {
#ifdef _WIN32
  CAMLparam1(unit);
  DWORD pid = GetCurrentProcessId();
  if ((uintnat)pid > (uintnat)Max_long) {
    caml_failwith("Windows process ID does not fit in an OCaml int");
  }
  CAMLreturn(Val_long(pid));
#else
  caml_invalid_argument("Windows process IDs are only available on Windows");
#endif
}

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
#ifndef SYS___pthread_fchdir
#define SYS___pthread_fchdir 349
#endif

static int __pthread_chdir(const char *path) {
#pragma clang diagnostic push
#pragma clang diagnostic ignored "-Wdeprecated"
  return syscall(SYS___pthread_chdir, path);
#pragma clang diagnostic pop
}

static int __pthread_fchdir(int fd) {
#pragma clang diagnostic push
#pragma clang diagnostic ignored "-Wdeprecated"
  return syscall(SYS___pthread_fchdir, fd);
#pragma clang diagnostic pop
}

CAMLprim value dune_pthread_chdir(value dir) {
  CAMLparam1(dir);
  if (__pthread_chdir(String_val(dir))) {
    uerror("__pthread_chdir", dir);
  }
  CAMLreturn(Val_unit);
}

CAMLprim value dune_reset_pthread_cwd(value unit) {
  CAMLparam1(unit);
  if (__pthread_fchdir(-1)) {
    uerror("__pthread_fchdir", Nothing);
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

CAMLprim value dune_reset_pthread_cwd(value unit) {
  CAMLparam1(unit);
  caml_invalid_argument("__pthread_fchdir not implemented");
  CAMLreturn(Val_unit);
}

#endif

#ifndef _WIN32
#include <errno.h>
#include <fcntl.h>
#include <string.h>
#include <sys/socket.h>
#include <unistd.h>

CAMLprim value dune_rpc_send_fds(value v_socket, value v_fds) {
  CAMLparam2(v_socket, v_fds);
  int fds[3];
  mlsize_t count = Wosize_val(v_fds);
  if (count == 0 || count > sizeof(fds) / sizeof(*fds)) {
    caml_invalid_argument("dune_rpc_send_fds");
  }
  for (mlsize_t i = 0; i < count; i++) {
    fds[i] = Int_val(Field(v_fds, i));
  }
  char marker = '\0';
  struct iovec iov = {.iov_base = &marker, .iov_len = 1};
  union {
    struct cmsghdr align;
    char bytes[CMSG_SPACE(sizeof(fds))];
  } control = {0};
  struct msghdr msg = {.msg_iov = &iov,
                       .msg_iovlen = 1,
                       .msg_control = control.bytes,
                       .msg_controllen = CMSG_SPACE(count * sizeof(int))};
  struct cmsghdr *cmsg = CMSG_FIRSTHDR(&msg);
  cmsg->cmsg_level = SOL_SOCKET;
  cmsg->cmsg_type = SCM_RIGHTS;
  cmsg->cmsg_len = CMSG_LEN(count * sizeof(int));
  memcpy(CMSG_DATA(cmsg), fds, count * sizeof(int));
  int flags = 0;
#ifdef MSG_NOSIGNAL
  flags |= MSG_NOSIGNAL;
#endif
  ssize_t len = sendmsg(Int_val(v_socket), &msg, flags);
  if (len == -1) {
    uerror("sendmsg", Nothing);
  }
  if (len != 1) {
    caml_failwith("sendmsg did not send the RPC descriptor marker");
  }
  CAMLreturn(Val_unit);
}

CAMLprim value dune_rpc_recv_fds(value v_socket, value v_bytes, value v_pos,
                                 value v_len) {
  CAMLparam4(v_socket, v_bytes, v_pos, v_len);
  CAMLlocal3(result, list, cell);
  int fds[256];
  union {
    struct cmsghdr align;
    char bytes[CMSG_SPACE(sizeof(fds))];
  } control;
  struct iovec iov = {.iov_base = &Byte(v_bytes, Long_val(v_pos)),
                      .iov_len = Long_val(v_len)};
  struct msghdr msg = {.msg_iov = &iov,
                       .msg_iovlen = 1,
                       .msg_control = control.bytes,
                       .msg_controllen = sizeof(control.bytes)};
  int flags = 0;
#ifdef MSG_CMSG_CLOEXEC
  flags |= MSG_CMSG_CLOEXEC;
#endif
  ssize_t len = recvmsg(Int_val(v_socket), &msg, flags);
  if (len == -1) {
    uerror("recvmsg", Nothing);
  }
  size_t count = 0;
  for (struct cmsghdr *cmsg = CMSG_FIRSTHDR(&msg); cmsg;
       cmsg = CMSG_NXTHDR(&msg, cmsg)) {
    if (cmsg->cmsg_level == SOL_SOCKET && cmsg->cmsg_type == SCM_RIGHTS) {
      size_t size = cmsg->cmsg_len - CMSG_LEN(0);
      memcpy(fds + count, CMSG_DATA(cmsg), size);
      count += size / sizeof(int);
    }
  }
  int error = msg.msg_flags & MSG_CTRUNC ? EMSGSIZE : 0;
#ifndef MSG_CMSG_CLOEXEC
  for (size_t i = 0; i < count && !error; i++) {
    if (fcntl(fds[i], F_SETFD, FD_CLOEXEC) == -1) {
      error = errno;
    }
  }
#endif
  if (error) {
    for (size_t i = 0; i < count; i++) {
      close(fds[i]);
    }
    errno = error;
    uerror("recvmsg", Nothing);
  }
  list = Val_emptylist;
  while (count > 0) {
    cell = caml_alloc_small(2, 0);
    Field(cell, 0) = Val_int(fds[--count]);
    Field(cell, 1) = list;
    list = cell;
  }
  result = caml_alloc_small(2, 0);
  Field(result, 0) = Val_long(len);
  Field(result, 1) = list;
  CAMLreturn(result);
}
#else
CAMLprim value dune_rpc_send_fds(value v_socket, value v_fds) {
  (void)v_socket;
  (void)v_fds;
  caml_invalid_argument("RPC file descriptor passing is not supported on Windows");
}

CAMLprim value dune_rpc_recv_fds(value v_socket, value v_bytes, value v_pos,
                                 value v_len) {
  (void)v_socket;
  (void)v_bytes;
  (void)v_pos;
  (void)v_len;
  caml_invalid_argument("RPC file descriptor passing is not supported on Windows");
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
