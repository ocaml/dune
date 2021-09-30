
#include <caml/alloc.h>
#include <caml/memory.h>
#include <caml/mlvalues.h>
#include <caml/unixsupport.h>
#include <string.h>

#include <errno.h>

#ifndef _WIN32

#include <caml/signals.h>

#include <errno.h>
#include <sys/types.h>
#include <dirent.h>
typedef struct dirent directory_entry;

int dune_scandir_select(const directory_entry* dirent) {
  if (strcmp(dirent->d_name, ".") == 0 || strcmp(dirent->d_name, "..") == 0) {
    return 0;
  } else {
    return 1;
  }
}

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

CAMLprim value dune_scandir(value v_dir) {
  CAMLparam1(v_dir);
  char* dir = caml_stat_strdup(String_val(v_dir));
  caml_enter_blocking_section();
  errno = 0;
  directory_entry **namelist;
  int n = scandir(dir, &namelist, &dune_scandir_select, NULL);
  caml_leave_blocking_section();
  if (n == -1) {
    if(errno == 0) {
      CAMLreturn(Val_int(0));
    } else {
      uerror("scandir", Nothing);
    }
  }

  CAMLlocal4(v_entries_xs, v_entries_x, v_tuple, v_filename);
  while(n--) {
    v_filename = caml_copy_string(namelist[n]->d_name);
    v_tuple = caml_alloc_small(2, 0);
    Field(v_tuple, 0) = v_filename;
#ifndef __HAIKU__
    Field(v_tuple, 1) = val_file_type(namelist[n]->d_type);
#else
    Field(v_tuple, 1) = Val_int(7);
#endif
    free(namelist[n]);

    v_entries_x = caml_alloc(2, 0);
    Store_field(v_entries_x, 0, v_tuple);
    Store_field(v_entries_x, 1, v_entries_xs);
    v_entries_xs = v_entries_x;
  }
  free(namelist);
  CAMLreturn(v_entries_xs);
}

#else
CAMLprim value dune_scandir(value vd)
{
  unix_error(ENOSYS, "scandir", Nothing);
}
#endif
