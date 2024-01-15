#define CAML_INTERNALS  // needed to access md5.h functions
#include <caml/mlvalues.h>
#include <caml/alloc.h>
#include <caml/bigarray.h>
#include <caml/memory.h>
#include <caml/signals.h>
#include <caml/threads.h>
#include <caml/unixsupport.h>
#include <errno.h>
#include <caml/md5.h>

/* yanked from:
 * https://github.com/janestreet/core/blob/master/core/src/md5_stubs.c
 */
CAMLprim value dune_md5_fd(value v_fd) {
  CAMLparam1(v_fd);
  CAMLlocal1(v_res);
#ifdef _WIN32
  int fd = win_CRT_fd_of_filedescr(v_fd);
#else
  int fd = Int_val(v_fd);
#endif
  struct MD5Context ctx;
  caml_release_runtime_system();
  {
    char buffer[UNIX_BUFFER_SIZE];

    intnat bytes_read;
    caml_MD5Init(&ctx);
    while (1) {
      bytes_read = read(fd, buffer, sizeof(buffer));
      if (bytes_read == 0) {
        break;
      } else if (bytes_read < 0) {
        if (errno == EINTR)
          continue;
        caml_acquire_runtime_system();
        uerror("read", Nothing);
      } else {
        caml_MD5Update(&ctx, (unsigned char *)buffer, bytes_read);
      }
    }
  }
  caml_acquire_runtime_system();
  v_res = caml_alloc_string(16);
  caml_MD5Final(&Byte_u(v_res, 0), &ctx);
  CAMLreturn(v_res);
}
