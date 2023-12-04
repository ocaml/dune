/*
 *	Copyright (C) 2006-2009 Vincent Hanquez <tab@snarc.org>
 *
 * Permission to use, copy, modify, and distribute this software for any
 * purpose with or without fee is hereby granted, provided that the above
 * copyright notice and this permission notice appear in all copies.
 *
 * THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
 * WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
 * MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
 * ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
 * WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
 * ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
 * OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.
 *
 * SHA1 implementation as describe in wikipedia.
 */

#define _GNU_SOURCE
#ifdef _MSC_VER
#include <BaseTsd.h>
typedef SSIZE_T ssize_t;
#define alloca _alloca
#else
#include <unistd.h>
#endif
#include <string.h>
#include <fcntl.h>
#include <string.h>
#include "sha1.h"

static inline int sha1_file(char *filename, sha1_digest *digest)
{
	#define BLKSIZE 4096
	unsigned char buf[BLKSIZE];
	int fd; ssize_t n;
	struct sha1_ctx ctx;

#ifndef O_CLOEXEC
	fd = open(filename, O_RDONLY);
#else
	fd = open(filename, O_RDONLY | O_CLOEXEC);
#endif
	if (fd == -1)
		return 1;
	sha1_init(&ctx);
	while ((n = read(fd, buf, BLKSIZE)) > 0)
		sha1_update(&ctx, buf, n);
	if (n == 0)
		sha1_finalize(&ctx, digest);
	close(fd);
	return n < 0;
	#undef BLKSIZE
}

/* this part implement the OCaml binding */
#include <caml/mlvalues.h>
#include <caml/memory.h>
#include <caml/alloc.h>
#include <caml/custom.h>
#include <caml/fail.h>
#include <caml/bigarray.h>
#include <caml/threads.h>

#include <caml/version.h>
#if OCAML_VERSION < 40600
#define Bytes_val(x) ((unsigned char *) Bp_val(x))
#endif

#define GET_CTX_STRUCT(a) ((struct sha1_ctx *) a)

CAMLexport value stub_sha1_init(value unit)
{
	CAMLparam1(unit);
	CAMLlocal1(result);

	result = caml_alloc(sizeof(struct sha1_ctx), Abstract_tag);
	sha1_init(GET_CTX_STRUCT(result));

	CAMLreturn(result);
}

CAMLprim value stub_sha1_update(value ctx, value data, value ofs, value len)
{
	CAMLparam4(ctx, data, ofs, len);

	sha1_update(GET_CTX_STRUCT(ctx), (unsigned char *) data + Int_val(ofs),
	            Int_val(len));

	CAMLreturn(Val_unit);
}

CAMLprim value stub_sha1_update_bigarray(value ctx, value buf)
{
	CAMLparam2(ctx, buf);
	struct sha1_ctx ctx_dup;
	unsigned char *data = Caml_ba_data_val(buf);
	size_t len = Caml_ba_array_val(buf)->dim[0];

	ctx_dup = *GET_CTX_STRUCT(ctx);
	caml_release_runtime_system();
	sha1_update(&ctx_dup, data, len);
	caml_acquire_runtime_system();
	*GET_CTX_STRUCT(ctx) = ctx_dup;

	CAMLreturn(Val_unit);
}


CAMLprim value stub_sha1_finalize(value ctx)
{
	CAMLparam1(ctx);
	CAMLlocal1(result);

	result = caml_alloc(sizeof(sha1_digest), Abstract_tag);
	sha1_finalize(GET_CTX_STRUCT(ctx), (sha1_digest *) result);

	CAMLreturn(result);
}

CAMLprim value stub_sha1_copy(value ctx)
{
	CAMLparam1(ctx);
	CAMLlocal1(result);

	result = caml_alloc(sizeof(struct sha1_ctx), Abstract_tag);
	sha1_copy(GET_CTX_STRUCT(result), GET_CTX_STRUCT(ctx));

	CAMLreturn(result);
}

#ifndef strdupa
#define strdupa(s) strcpy(alloca(strlen(s)+1),s)
#endif

CAMLprim value stub_sha1_file(value name)
{
	CAMLparam1(name);
	CAMLlocal1(result);

	char *name_dup = strdupa(String_val(name));
	sha1_digest digest;

	caml_release_runtime_system();
	if (sha1_file(name_dup, &digest)) {
	    caml_acquire_runtime_system();
	    caml_failwith("file error");
	}
	caml_acquire_runtime_system();
	result = caml_alloc(sizeof(sha1_digest), Abstract_tag);
	memcpy((sha1_digest *)result, &digest, sizeof(sha1_digest));

	CAMLreturn(result);
}

CAMLprim value stub_sha1_to_bin(value digest)
{
	CAMLparam1(digest);
	CAMLlocal1(result);

	result = caml_alloc_string(20);
	sha1_to_bin((sha1_digest *) digest, &Byte(result, 0));

	CAMLreturn(result);
}

CAMLprim value stub_sha1_to_hex(value digest)
{
	CAMLparam1(digest);
	CAMLlocal1(result);

	result = caml_alloc_string(40);
	sha1_to_hex((sha1_digest *) digest, &Byte(result, 0));

	CAMLreturn(result);
}

CAMLprim value stub_sha1_equal(value t1, value t2)
{
	CAMLparam2(t1, t2);
	int b = memcmp((sha1_digest *) t1, (sha1_digest *) t2, sizeof(sha1_digest)) == 0;
	CAMLreturn(Val_bool(b));
}

CAMLprim value stub_sha1_of_bin(value bin)
{
	CAMLparam1(bin);
	CAMLlocal1(result);

	result = caml_alloc(sizeof(sha1_digest), Abstract_tag);
	sha1_of_bin((const char *) Bytes_val(bin), (sha1_digest *) result);

	CAMLreturn(result);
}

CAMLprim value stub_sha1_of_hex(value hex)
{
	CAMLparam1(hex);
	CAMLlocal1(result);

	result = caml_alloc(sizeof(sha1_digest), Abstract_tag);
	sha1_of_hex(String_val(hex), (sha1_digest *) result);

	CAMLreturn(result);
}
