
#include <caml/alloc.h>
#include <caml/memory.h>
#include <caml/mlvalues.h>
#include <caml/unixsupport.h>

#include <errno.h>

#ifndef _WIN32

#include <caml/signals.h>

#include <errno.h>
#include <sys/types.h>
#include <dirent.h>
typedef struct dirent directory_entry;

value val_file_type(int typ) {
  switch(typ)
    {
#ifndef __HAIKU__     
   case DT_REG:
      return Val_int(0);
    case DT_DIR:
      return Val_int(1);
    case DT_CHR:
      return Val_int(2);
    case DT_BLK:
      return Val_int(3);
    case DT_LNK:
      return Val_int(4);
    case DT_FIFO:
      return Val_int(5);
    case DT_SOCK:
      return Val_int(6);
    case DT_UNKNOWN:
      return Val_int(7);
#endif
    default:
      return Val_int(7);
    }
}

CAMLprim value caml__dune_filesystem_stubs__readdir(value vd)
{
  CAMLparam1(vd);
  CAMLlocal2(v_filename, v_tuple);

  DIR * d;
  directory_entry * e;
  d = DIR_Val(vd);
  if (d == (DIR *) NULL) unix_error(EBADF, "readdir", Nothing);
  caml_enter_blocking_section();
  errno = 0;
  e = readdir((DIR *) d);
  caml_leave_blocking_section();
  if (e == (directory_entry *) NULL) {
    if(errno == 0) {
      CAMLreturn(Val_int(0));
    } else {
      uerror("readdir", Nothing);
    }
  }
  v_filename = caml_copy_string(e->d_name);
  v_tuple = caml_alloc_small(2, 0);
  Field(v_tuple, 0) = v_filename;
#ifndef __HAIKU__
  Field(v_tuple, 1) = val_file_type(e->d_type);
#else
  Field(v_tuple, 1) = Val_int(7);
#endif
  CAMLreturn(v_tuple);
}

#else
CAMLprim value caml__dune_filesystem_stubs__readdir(value vd)
{
  unix_error(ENOSYS, "readdir", Nothing);
}
#endif
