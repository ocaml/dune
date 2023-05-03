/*
 	Copyright (C) 2006-2009 Vincent Hanquez <tab@snarc.org>
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
 * SHA256 implementation
 */

#ifndef SHA256_H
#define SHA256_H

#include <string.h>
#include <stdio.h>
#include "bitfn.h"
#include "util.h"

struct sha256_ctx
{
	unsigned int h[8];
	unsigned char buf[128];
	unsigned long long sz;
};

typedef struct { unsigned int digest[8]; } sha256_digest;

/**
 * sha256_init - Init SHA256 context
 */
static void sha256_init(struct sha256_ctx *ctx)
{
	memset(ctx, 0, sizeof(*ctx));

	ctx->h[0] = 0x6a09e667;
	ctx->h[1] = 0xbb67ae85;
	ctx->h[2] = 0x3c6ef372;
	ctx->h[3] = 0xa54ff53a;
	ctx->h[4] = 0x510e527f;
	ctx->h[5] = 0x9b05688c;
	ctx->h[6] = 0x1f83d9ab;
	ctx->h[7] = 0x5be0cd19;
}

/**
 * sha256_copy - Copy SHA256 context
 */
static void sha256_copy(struct sha256_ctx *dst, struct sha256_ctx *src)
{
	memcpy(dst, src, sizeof(*dst));
}

/* 232 times the cube root of the first 64 primes 2..311 */
static const unsigned int k[] = {
	0x428a2f98, 0x71374491, 0xb5c0fbcf, 0xe9b5dba5, 0x3956c25b, 0x59f111f1,
	0x923f82a4, 0xab1c5ed5, 0xd807aa98, 0x12835b01, 0x243185be, 0x550c7dc3,
	0x72be5d74, 0x80deb1fe, 0x9bdc06a7, 0xc19bf174, 0xe49b69c1, 0xefbe4786,
	0x0fc19dc6, 0x240ca1cc, 0x2de92c6f, 0x4a7484aa, 0x5cb0a9dc, 0x76f988da,
	0x983e5152, 0xa831c66d, 0xb00327c8, 0xbf597fc7, 0xc6e00bf3, 0xd5a79147,
	0x06ca6351, 0x14292967, 0x27b70a85, 0x2e1b2138, 0x4d2c6dfc, 0x53380d13,
	0x650a7354, 0x766a0abb, 0x81c2c92e, 0x92722c85, 0xa2bfe8a1, 0xa81a664b,
	0xc24b8b70, 0xc76c51a3, 0xd192e819, 0xd6990624, 0xf40e3585, 0x106aa070,
	0x19a4c116, 0x1e376c08, 0x2748774c, 0x34b0bcb5, 0x391c0cb3, 0x4ed8aa4a,
	0x5b9cca4f, 0x682e6ff3, 0x748f82ee, 0x78a5636f, 0x84c87814, 0x8cc70208,
	0x90befffa, 0xa4506ceb, 0xbef9a3f7, 0xc67178f2 };

static inline unsigned int Ch(unsigned int x, unsigned int y, unsigned int z)
{
	return z ^ (x & (y ^ z));
}

static inline unsigned int Maj(unsigned int x, unsigned int y, unsigned int z)
{
	return (x & y) | (z & (x | y));
}

#define e0(x)       (ror32(x, 2) ^ ror32(x,13) ^ ror32(x,22))
#define e1(x)       (ror32(x, 6) ^ ror32(x,11) ^ ror32(x,25))
#define s0(x)       (ror32(x, 7) ^ ror32(x,18) ^ (x >> 3))
#define s1(x)       (ror32(x,17) ^ ror32(x,19) ^ (x >> 10))

/**
 * sha256_do_chunk - Process a block through SHA256
 */
static void sha256_do_chunk(unsigned char __W[], unsigned int H[])
{
	unsigned int a, b, c, d, e, f, g, h, t1, t2;
	unsigned int W[64];
	int i;

	for (i = 0; i < 16; i++)
		W[i] = be32_to_cpu(((unsigned int *) __W)[i]);

	for (i = 16; i < 64; i++)
		W[i] = s1(W[i - 2]) + W[i - 7] + s0(W[i - 15]) + W[i - 16];

	a = H[0];
	b = H[1];
	c = H[2];
	d = H[3];
	e = H[4];
	f = H[5];
	g = H[6];
	h = H[7];

#define T(a, b, c, d, e, f, g, h, k, w)			\
	do {						\
		t1 = h + e1(e) + Ch(e, f, g) + k + w;	\
		t2 = e0(a) + Maj(a, b, c);		\
		d += t1;				\
		h = t1 + t2;				\
	} while (0)

#define PASS(i)							\
	do {							\
		T(a, b, c, d, e, f, g, h, k[i + 0], W[i + 0]);	\
		T(h, a, b, c, d, e, f, g, k[i + 1], W[i + 1]);	\
		T(g, h, a, b, c, d, e, f, k[i + 2], W[i + 2]);	\
		T(f, g, h, a, b, c, d, e, k[i + 3], W[i + 3]);	\
		T(e, f, g, h, a, b, c, d, k[i + 4], W[i + 4]);	\
		T(d, e, f, g, h, a, b, c, k[i + 5], W[i + 5]);	\
		T(c, d, e, f, g, h, a, b, k[i + 6], W[i + 6]);	\
		T(b, c, d, e, f, g, h, a, k[i + 7], W[i + 7]);	\
	} while (0)

	PASS(0);
	PASS(8);
	PASS(16);
	PASS(24);
	PASS(32);
	PASS(40);
	PASS(48);
	PASS(56);

#undef T
#undef PASS

	H[0] += a;
	H[1] += b;
	H[2] += c;
	H[3] += d;
	H[4] += e;
	H[5] += f;
	H[6] += g;
	H[7] += h;
}

/**
 * sha256_update - Update the SHA256 context values with length bytes of data
 */
static void sha256_update(struct sha256_ctx *ctx, unsigned char *data, int len)
{
	unsigned int index, to_fill;

	/* check for partial buffer */
	index = (unsigned int) (ctx->sz & 0x3f);
	to_fill = 64 - index;

	ctx->sz += len;

	/* process partial buffer if there's enough data to make a block */
	if (index && len >= to_fill) {
		memcpy(ctx->buf + index, data, to_fill);
		sha256_do_chunk(ctx->buf, ctx->h);
		len -= to_fill;
		data += to_fill;
		index = 0;
	}

	/* process as much 64-block as possible */
	for (; len >= 64; len -= 64, data += 64)
		sha256_do_chunk(data, ctx->h);

	/* append data into buf */
	if (len)
		memcpy(ctx->buf + index, data, len);
}

/**
 * sha256_finalize - Finalize the context and create the SHA256 digest
 */
static void sha256_finalize(struct sha256_ctx *ctx, sha256_digest *out)
{
	static unsigned char padding[64] = { 0x80, };
	unsigned int bits[2];
	unsigned int i, index, padlen;

	/* cpu -> big endian */
	bits[0] = cpu_to_be32((unsigned int) (ctx->sz >> 29));
	bits[1] = cpu_to_be32((unsigned int) (ctx->sz << 3));

	/* pad out to 56 */
	index = (unsigned int) (ctx->sz & 0x3f);
	padlen = (index < 56) ? (56 - index) : ((64 + 56) - index);
	sha256_update(ctx, padding, padlen);

	/* append length */
	sha256_update(ctx, (unsigned char *) bits, sizeof(bits));

	/* store to digest */
	for (i = 0; i < 8; i++)
		out->digest[i] = cpu_to_be32(ctx->h[i]);
}

/**
 * sha256_to_bin - Transform the SHA256 digest into a binary data
 */
static void sha256_to_bin(sha256_digest *digest, char *out)
{
	uint32_t *ptr = (uint32_t *) out;
	int i;

	for (i = 0; i < 8; i++)
		ptr[i] = digest->digest[i];
}

/**
 * sha256_to_hex - Transform the SHA256 digest into a readable data
 */
static void sha256_to_hex(sha256_digest *digest, char *out)
{
	char *p;
	int i;

	for (p = out, i = 0; i < 8; i++, p += 8)
		snprintf(p, 9, "%08x", be32_to_cpu(digest->digest[i]));
}

/**
 * sha256_of_bin - Transform binary data into the SHA256 digest
 */
static void sha256_of_bin(const char *in, sha256_digest *digest)
{
	memcpy(digest->digest, in, sizeof(*digest));
}

/**
 * sha256_of_hex - Transform readable data into the SHA256 digest
 */
static void sha256_of_hex(const char *in, sha256_digest *digest)
{
	if (strlen(in) != 64)
		return;
	of_hex((unsigned char *) digest->digest, in, 64);
}

#endif
