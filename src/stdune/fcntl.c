#include <fcntl.h>
#include <stdio.h>

#include <caml/custom.h>
#include <caml/mlvalues.h>
#include <caml/fail.h>
#include <caml/alloc.h>
#include <caml/memory.h>

CAMLprim value fcntl_lk_native(value fd, value op_v, value type_v, value whence_v, value start, value len)
{
  int op;
  switch (Int_val(op_v))
  {
  case 0:
    op = F_SETLK;
    break;
  case 1:
    op = F_SETLKW;
    break;
  case 2:
    op = F_GETLK;
    break;
  default:
    caml_failwith("fcntl: invalid lock operation");
  }
  int type;
  switch (Int_val(type_v))
  {
  case 0:
    type = F_RDLCK;
    break;
  case 1:
    type = F_WRLCK;
    break;
  case 2:
    type = F_UNLCK;
    break;
  default:
    caml_failwith("fcntl: invalid lock type");
  }
  int whence;
  switch (Int_val(whence_v))
  {
  case 0:
    whence = SEEK_SET;
    break;
  case 1:
    whence = SEEK_CUR;
    break;
  case 2:
    whence = SEEK_END;
    break;
  default:
    caml_failwith("fcntl: invalid lock operation");
  }
  struct flock l = {
    .l_type = type,
    .l_whence = whence,
    .l_start = Long_val(start),
    .l_len = Long_val(len),
  };

  int rv = fcntl(Int_val(fd), op, &l);

  value res;

  res = caml_alloc(2, 0);

  if (op == F_GETLK)
  {
    switch (l.l_type)
    {
      case F_UNLCK:
        Store_field(res, 0, Val_int(0));
        Store_field(res, 1, Val_int(0));
        break;
      case F_RDLCK:
        Store_field(res, 0, Val_int(1));
        Store_field(res, 1, Val_int(l.l_pid));
        break;
      case F_WRLCK:
        Store_field(res, 0, Val_int(2));
        Store_field(res, 1, Val_int(l.l_pid));
        break;
      default:
        Store_field(res, 0, Val_int(-1));
        Store_field(res, 1, Val_int(0));
        break;
    }
  }
  else
  {
    Store_field(res, 0, Val_int(rv));
    Store_field(res, 1, Val_int(0));
  }
  return res;
}

CAMLprim value fcntl_lk_bytecode(value* argv, int argn)
{
  return fcntl_lk_native(argv[0], argv[1], argv[2], argv[3], argv[4], argv[5]);
}
