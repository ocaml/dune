/*
 * Copyright (c) 2019, Christopher Zimmermann
 *
 * Permission to use, copy, modify, and/or distribute this software for any
 * purpose with or without fee is hereby granted, provided that the above
 * copyright notice and this permission notice appear in all copies.
 *
 * THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
 * WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
 * MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR ANY
 * SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
 * WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN ACTION
 * OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF OR IN
 * CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.
 */


#ifdef _WIN32
#include <malloc.h>
#elif defined(__FreeBSD__) || defined(__OpenBSD__) || defined(__NetBSD__) || defined(__DragonFlyBSD__)
#include <stdlib.h>
#else
#include <alloca.h>
#endif

#include <string.h>
#include <errno.h>
#include <lmdb.h>

#define CAML_NAME_SPACE
#include <caml/mlvalues.h>
#include <caml/threads.h>
#include <caml/alloc.h>
#include <caml/memory.h>
#include <caml/fail.h>
#include <caml/bigarray.h>
#include <caml/misc.h>
#include <caml/callback.h>
#include <caml/unixsupport.h>


int constants[] = {
  MDB_APPEND,
  MDB_APPENDDUP,
  MDB_CP_COMPACT,
  MDB_CREATE,
  MDB_CURRENT,
  MDB_DUPFIXED,
  MDB_DUPSORT,
  MDB_FIRST,
  MDB_FIRST_DUP,
  MDB_FIXEDMAP,
  MDB_GET_BOTH,
  MDB_GET_BOTH_RANGE,
  MDB_GET_CURRENT,
  MDB_GET_MULTIPLE,
  MDB_INTEGERDUP,
  MDB_INTEGERKEY,
  MDB_LAST,
  MDB_LAST_DUP,
  MDB_MAPASYNC,
  MDB_MULTIPLE,
  MDB_NEXT,
  MDB_NEXT_DUP,
  MDB_NEXT_MULTIPLE,
  MDB_NEXT_NODUP,
  MDB_NODUPDATA,
  MDB_NOLOCK,
  MDB_NOMEMINIT,
  MDB_NOMETASYNC,
  MDB_NOOVERWRITE,
  MDB_NORDAHEAD,
  MDB_NOSUBDIR,
  MDB_NOSYNC,
  MDB_NOTLS,
  MDB_PREV,
  MDB_PREV_DUP,
  /* MDB_PREV_MULTIPLE - only since 0.9.19 */
  MDB_PREV_NODUP,
  MDB_RDONLY,
  MDB_RESERVE,
  MDB_REVERSEDUP,
  MDB_REVERSEKEY,
  MDB_SET,
  MDB_SET_KEY,
  MDB_SET_RANGE,
  MDB_WRITEMAP,
  sizeof(int),
  sizeof(size_t),
};

const value *exn_exists;
const value *exn_map_full;
const value *exn_error;


void mdbs_assert_func(MDB_env *env, const char *msg) {
  const char *path;
  mdb_env_get_path(env, &path);
  char *text = alloca(39 + strlen(path) + strlen(msg));

  strcpy(text, "Lmdb backend assertion failure in ");
  strcat(text, path);
  strcat(text, " at ");
  strcat(text, msg);

  caml_acquire_runtime_system();
  caml_failwith(text);
}

CAMLprim value mdbs_init(value unit)
{
  CAMLparam0();
  CAMLlocal4(version, string, array, pair);
  int major, minor, patch;
  unsigned i;

  exn_exists = caml_named_value("LmdbExists");
  exn_map_full = caml_named_value("LmdbMapFull");
  exn_error  = caml_named_value("LmdbError");

  string = caml_copy_string(mdb_version(&major, &minor, &patch));

  version = caml_alloc_small(4, 0);
  Field(version, 0) = string;
  Field(version, 1) = Val_int(major);
  Field(version, 2) = Val_int(minor);
  Field(version, 3) = Val_int(patch);

  CAMLassert(sizeof(constants) / sizeof(value) <= Max_young_wosize);
  array = caml_alloc_small(sizeof(constants) / sizeof(constants[0]), 0);
  for (i = 0; i < Wosize_val(array); i++)
    Field(array, i) = Val_int(constants[i]);

  pair = caml_alloc_small(2, 0);
  Field(pair, 0) = version;
  Field(pair, 1) = array;

  CAMLreturn(pair);
}

CAMLprim value mdbs_strerror(value errn)
{
  return caml_copy_string(mdb_strerror(Int_val(errn)));
}

void mdbs_err(int errn)
{
  switch (errn)
  {
    case MDB_SUCCESS:
      return;
    case MDB_NOTFOUND:
      caml_raise_not_found();
    case MDB_KEYEXIST:
      caml_raise_constant(*exn_exists);
    case MDB_MAP_FULL:
      caml_raise_constant(*exn_map_full);
    case EINVAL:
      caml_invalid_argument("Lmdb");
    default:
      caml_raise_with_arg(*exn_error, Val_int(errn));
  }
}

#define mdbs_err_rel(ferr) \
  do { \
    int errn; \
    caml_release_runtime_system(); \
    errn = (ferr); \
    caml_acquire_runtime_system(); \
    if (errn) mdbs_err(errn); \
  } while(0)

static inline value hide(void *p)
{
  CAMLassert(((intnat)p & 1) == 0);
  return ((intnat)p | 1);
}
static inline void *unhide(value v)
{
  CAMLassert(((intnat)v & 1) == 1);
  return ((void *)((v) & ~1));
}

CAMLprim value mdbs_env_create(value unit)
{
  MDB_env *env;
  mdbs_err_rel(mdb_env_create(&env));
  mdb_env_set_assert(env, mdbs_assert_func);

  return hide(env);
}

CAMLprim value mdbs_env_open(value env, value path, value flags, value mode)
{
  size_t cpath_length = caml_string_length(path) + 1;
  char *cpath = alloca(cpath_length);
  memcpy(cpath, String_val(path), cpath_length);

  mdbs_err_rel(mdb_env_open(
	unhide(env),
	cpath,
	Unsigned_int_val(flags),
	Int_val(mode)));

  return Val_unit;
}

CAMLprim value mdbs_env_close(value env)
{
  caml_release_runtime_system();
  mdb_env_close(unhide(env));
  caml_acquire_runtime_system();

  return Val_unit;
}

#define set(name, type) \
CAMLprim value mdbs_ ## name (value ctx, value x) { \
  mdbs_err_rel(mdb_ ## name (unhide(ctx), (type)Long_val(x))); \
  return Val_unit; }

#define get(name, type) \
CAMLprim value mdbs_ ## name (value ctx) { \
  type x; \
  mdbs_err_rel(mdb_ ## name (unhide(ctx), &x)); \
  return Val_int(x); }

set(env_set_mapsize, uintnat)
set(env_set_maxdbs, unsigned)
set(env_set_maxreaders, unsigned)
get(env_get_maxreaders, unsigned)
get(env_get_flags, unsigned)
get(reader_check, int)
get(cursor_count, size_t)


CAMLprim value mdbs_env_get_maxkeysize(value env)
{
  caml_release_runtime_system();
  int ret = Val_int(mdb_env_get_maxkeysize(unhide(env)));
  caml_acquire_runtime_system();
  return ret;
}

CAMLprim value mdbs_env_set_flags(value env, value flags, value onoff)
{
  mdbs_err_rel(mdb_env_set_flags(
	unhide(env),
	Unsigned_long_val(flags),
	Unsigned_long_val(onoff)));

  return Val_unit;
}

CAMLprim value mdbs_dbi_flags(value txn, value dbi) {
  unsigned flags;
  mdbs_err_rel(mdb_dbi_flags(
	unhide(txn),
	Unsigned_int_val(dbi),
	&flags));
  return Val_int(flags);
}

int mdbs_msg_func(const char *msg, void *callback)
{
  int ret;
  caml_acquire_runtime_system();
  ret = Int_val(caml_callback(
	*(value *)callback,
	caml_copy_string(msg)));
  caml_release_runtime_system();
  return ret;
}

CAMLprim value mdbs_reader_list(value env, value callback)
{
  CAMLparam1(callback);

  caml_release_runtime_system();
  int ret =
    mdb_reader_list(
	unhide(env),
	&mdbs_msg_func,
	&callback);
  caml_acquire_runtime_system();

  if (ret < 0)
    mdbs_err(ret);

  CAMLreturn(Val_int(ret));
}

value make_stat(MDB_stat *cstat)
{
  value stat;

  stat = caml_alloc_small(6,0);
  Field(stat, 0) = Val_int (cstat->ms_psize);
  Field(stat, 1) = Val_int (cstat->ms_depth);
  Field(stat, 2) = Val_long(cstat->ms_branch_pages);
  Field(stat, 3) = Val_long(cstat->ms_leaf_pages);
  Field(stat, 4) = Val_long(cstat->ms_overflow_pages);
  Field(stat, 5) = Val_long(cstat->ms_entries);

  return stat;
}

CAMLprim value mdbs_env_stat(value env)
{
  MDB_stat cstat;

  mdbs_err_rel(mdb_env_stat(
	unhide(env),
	&cstat));

  return make_stat(&cstat);
}

CAMLprim value mdbs_stat(value txn, value dbi)
{
  MDB_stat cstat;

  mdbs_err_rel(mdb_stat(
	unhide(txn),
	Unsigned_int_val(dbi),
	&cstat));

  return make_stat(&cstat);
}

CAMLprim value mdbs_env_info(value env)
{
  MDB_envinfo cinfo;
  value info;

  mdbs_err_rel(mdb_env_info(
	unhide(env),
	&cinfo));

  info = caml_alloc_small(6,0);
  Field(info, 0) = hide(cinfo.me_mapaddr);
  Field(info, 1) = Val_long(cinfo.me_mapsize);
  Field(info, 2) = Val_long(cinfo.me_last_pgno);
  Field(info, 3) = Val_long(cinfo.me_last_txnid);
  Field(info, 4) = Val_int (cinfo.me_maxreaders);
  Field(info, 5) = Val_int (cinfo.me_numreaders);

  return info;
}

CAMLprim value mdbs_env_copy2(value env, value path, value flags)
{
  size_t cpath_length = caml_string_length(path) + 1;
  char *cpath = alloca(cpath_length);
  memcpy(cpath, String_val(path), cpath_length);

  mdbs_err_rel(mdb_env_copy2(
	unhide(env),
	cpath,
	Unsigned_int_val(flags)));

  return Val_unit;
}

CAMLprim value mdbs_env_copyfd2(value env, value fd, value flags)
{
#ifdef Handle_val
  CAMLparam1(fd);
  HANDLE cfd = Handle_val(fd);
  CAMLassert(Is_block(fd));
#else
  CAMLparam0();
  int cfd = Int_val(fd);
  CAMLassert(Is_long(fd));
#endif

  mdbs_err_rel(mdb_env_copyfd2(
	unhide(env),
	cfd,
	Unsigned_int_val(flags)));

  CAMLreturn(Val_unit);
}

CAMLprim value mdbs_env_get_path(value env)
{
  const char *path;

  mdbs_err_rel(mdb_env_get_path(unhide(env), &path));

  return caml_copy_string(path);
}

CAMLprim value mdbs_env_get_fd(value env)
{
  mdb_filehandle_t fd;

  mdbs_err_rel(mdb_env_get_fd(
	unhide(env),
	&fd));

#ifdef Handle_val
  return win_alloc_handle(fd);
#else
  return Val_int(fd);
#endif
}

CAMLprim value mdbs_env_sync(value env, value force)
{
  mdbs_err_rel(mdb_env_sync(unhide(env), Bool_val(force)));

  return Val_unit;
}

CAMLprim value mdbs_txn_env (value txn)
{
  MDB_env *env;
  caml_release_runtime_system();
  env = mdb_txn_env(unhide(txn));
  caml_acquire_runtime_system();

  if (env == NULL)
    caml_invalid_argument("Lmdb.Txn.env: invalid transaction handle.");

  return hide(env);
}

CAMLprim value mdbs_cursor_txn (value cursor)
{
  MDB_txn *txn;
  caml_release_runtime_system();
  txn = mdb_cursor_txn(unhide(cursor));
  caml_acquire_runtime_system();

  if (txn == NULL)
    caml_invalid_argument("Lmdb.Cursor.txn: invalid cursor handle.");

  return hide(txn);
}

CAMLprim value mdbs_txn_begin (value env, value parent, value flags)
{
  MDB_txn *cparent, *txn;

  if (Is_block(parent)) {
    /* Some */
    CAMLassert(Tag_val(parent) == 0);
    cparent = unhide(Field(parent,0));
  }
  else {
    /* None */
    CAMLassert(Int_val(parent) == 0);
    cparent = NULL;
  }

  /* mdb_txn_begin locks a mutex. Therefore it has to release the runtime.
   * Otherwise a deadlock involving the OCaml global runtime lock and the
   * lmdb writer lock could occur. */
  mdbs_err_rel(mdb_txn_begin(
	unhide(env),
	cparent,
	Unsigned_int_val(flags),
	&txn));

  return hide(txn);
}

CAMLprim value mdbs_cursor_open (value txn, value dbi)
{
  MDB_cursor *cursor;

  mdbs_err_rel(mdb_cursor_open(
	unhide(txn),
	Unsigned_int_val(dbi),
	&cursor));

  return hide(cursor);
}

CAMLprim value mdbs_txn_commit(value txn)
{
  mdbs_err_rel(mdb_txn_commit(unhide(txn)));
  return Val_unit;
}

CAMLprim value mdbs_cursor_close(value cursor)
{
  caml_release_runtime_system();
  mdb_cursor_close(unhide(cursor));
  caml_acquire_runtime_system();
  return Val_unit;
}

CAMLprim value mdbs_txn_abort(value txn)
{
  caml_release_runtime_system();
  mdb_txn_abort(unhide(txn));
  caml_acquire_runtime_system();
  return Val_unit;
}

CAMLprim value mdbs_dbi_open(value txn, value name, value flags)
{
  MDB_dbi dbi;

  size_t cname_length;
  char *cname;

  if (Is_block(name)) {
    CAMLassert(Tag_val(Field(name,0)) == String_tag);
    cname_length = caml_string_length(Field(name, 0)) + 1;
    cname = alloca(cname_length);
    memcpy(cname, String_val(Field(name,0)), cname_length);
  }
  else {
    CAMLassert(Int_val(name) == 0);
    cname_length = 0;
    cname = NULL;
  }

  mdbs_err_rel(mdb_dbi_open(
	unhide(txn),
	cname,
	Unsigned_int_val(flags),
	&dbi));

  return Val_int(dbi);
}

CAMLprim value mdbs_dbi_close(value env, value dbi)
{
  caml_release_runtime_system();
  mdb_dbi_close(unhide(env), Unsigned_int_val(dbi));
  caml_acquire_runtime_system();
  return Val_unit;
}


CAMLprim value mdbs_drop(value txn, value dbi, value del)
{
  mdbs_err_rel(mdb_drop(
	unhide(txn),
	Unsigned_int_val(dbi),
	Bool_val(del)));
  return Val_unit;
}

static inline void mvp_of_ba(MDB_val *mvp, value ba)
{
  struct caml_ba_array *cba = Caml_ba_array_val(ba);
  CAMLassert(cba->num_dims == 1);
  mvp->mv_size = cba->dim[0];
  mvp->mv_data = cba->data;
}

static inline void mvp_of_ba_opt(MDB_val *mvp, value opt)
{
  if (Is_block(opt)) {
    CAMLassert(Tag_val(opt) == Custom_tag);
    mvp_of_ba(mvp, opt);
  }
  else {
    mvp->mv_size = Unsigned_long_val(opt);
    mvp->mv_data = NULL;
  }
}

static inline value ba_of_mvp(MDB_val *mvp)
{
  return
    caml_ba_alloc(
#if 0
	CAML_BA_MAPPED_FILE |
#endif
	CAML_BA_CHAR | CAML_BA_C_LAYOUT | CAML_BA_EXTERNAL,
	1, mvp->mv_data, (intnat *)&mvp->mv_size);
}

CAMLprim value mdbs_get(value txn, value dbi, value key)
{
  CAMLparam1(key);
  MDB_val ckey, cval;

  mvp_of_ba(&ckey, key);

  mdbs_err_rel(mdb_get(
	unhide(txn),
	Unsigned_int_val(dbi),
	&ckey,
	&cval));

  CAMLreturn(ba_of_mvp(&cval));
}

CAMLprim value mdbs_cursor_get(value cursor, value keyopt, value valopt, value op)
{
  CAMLparam2(keyopt, valopt);
  CAMLlocal3(ret, rkey, rval);
  MDB_val ckey, cval;
  void *dkey, *dval;

  mvp_of_ba_opt(&ckey, keyopt);
  mvp_of_ba_opt(&cval, valopt);
  dkey = ckey.mv_data;
  dval = cval.mv_data;

  mdbs_err_rel(mdb_cursor_get(
	unhide(cursor),
	&ckey,
	&cval,
	Unsigned_int_val(op)));

  if (ckey.mv_data == dkey && Is_block(keyopt))
    rkey = keyopt;
  else
    rkey = ba_of_mvp(&ckey);
  if (cval.mv_data == dval && Is_block(valopt))
    rval = valopt;
  else
    rval = ba_of_mvp(&cval);

  ret = caml_alloc_small(2,0);
  Field(ret, 0) = rkey;
  Field(ret, 1) = rval;

  CAMLreturn(ret);
}

CAMLprim value mdbs_del(value txn, value dbi, value key, value valopt)
{
  CAMLparam2(key, valopt);
  MDB_val ckey, cval;

  mvp_of_ba(&ckey, key);

  if (Is_block(valopt)) {
    CAMLassert(Tag_val(valopt) == Custom_tag);
    mvp_of_ba(&cval, valopt);
  }

  mdbs_err_rel(mdb_del(
	unhide(txn),
	Unsigned_int_val(dbi),
	&ckey,
	Is_block(valopt) ? &cval : NULL));

  CAMLreturn(Val_unit);
}

CAMLprim value mdbs_cursor_del(value cursor, value flags)
{
  mdbs_err_rel(mdb_cursor_del(unhide(cursor), Unsigned_int_val(flags)));
  return Val_unit;
}

CAMLprim value mdbs_put(value txn, value dbi, value key, value valopt, value flags)
{
  CAMLparam2(key, valopt);
  MDB_val ckey, cval;

  mvp_of_ba(&ckey, key);
  mvp_of_ba_opt(&cval, valopt);

  mdbs_err_rel(mdb_put(
	unhide(txn),
	Unsigned_int_val(dbi),
	&ckey,
	&cval,
	Unsigned_int_val(flags) | (Is_block(valopt) ? 0 : MDB_RESERVE)));

  if (Is_block(valopt))
    CAMLreturn(Val_unit);
  else
    CAMLreturn(ba_of_mvp(&cval));
}

CAMLprim value mdbs_cursor_put(value cursor, value key, value valopt, value flags)
{
  CAMLparam2(key, valopt);
  MDB_val ckey, cval;

  mvp_of_ba(&ckey, key);
  mvp_of_ba_opt(&cval, valopt);

  mdbs_err_rel(mdb_cursor_put(
	unhide(cursor),
	&ckey,
	&cval,
	Unsigned_int_val(flags) | (Is_block(valopt) ? 0 : MDB_RESERVE)));

  if (Is_block(valopt))
    CAMLreturn(Val_unit);
  else
    CAMLreturn(ba_of_mvp(&cval));
}

CAMLprim value mdbs_cmp(value txn, value dbi, value key, value val)
{
  MDB_val ckey, cval;
  int ret;

  mvp_of_ba(&ckey, key);
  mvp_of_ba(&cval, val);

  caml_release_runtime_system();
  ret = Val_int(mdb_cmp(
	  unhide(txn),
	  Unsigned_int_val(dbi),
	  &ckey,
	  &cval));
  caml_acquire_runtime_system();

  return ret;
}

CAMLprim value mdbs_dcmp(value txn, value dbi, value key, value val)
{
  MDB_val ckey, cval;
  int ret;

  mvp_of_ba(&ckey, key);
  mvp_of_ba(&cval, val);

  caml_release_runtime_system();
  ret = Val_int(mdb_dcmp(
	  unhide(txn),
	  Unsigned_int_val(dbi),
	  &ckey,
	  &cval));
  caml_acquire_runtime_system();

  return ret;
}
