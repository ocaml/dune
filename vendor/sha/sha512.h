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
 * SHA512 implementation
 */

#ifndef SHA512_H
#define SHA512_H

#include <stdint.h>
#include <string.h>
#include <stdio.h>
#include "bitfn.h"
#include "util.h"

struct sha512_ctx
{
	uint64_t h[8];
	unsigned char buf[128];
	uint64_t sz[2];
};

typedef struct { uint64_t digest[8]; } sha512_digest;

/**
 * sha512_init - Init SHA512 context
 */
static void sha512_init(struct sha512_ctx *ctx)
{
	memset(ctx, 0, sizeof(*ctx));

	ctx->h[0] = 0x6a09e667f3bcc908ULL;
	ctx->h[1] = 0xbb67ae8584caa73bULL;
	ctx->h[2] = 0x3c6ef372fe94f82bULL;
	ctx->h[3] = 0xa54ff53a5f1d36f1ULL;
	ctx->h[4] = 0x510e527fade682d1ULL;
	ctx->h[5] = 0x9b05688c2b3e6c1fULL;
	ctx->h[6] = 0x1f83d9abfb41bd6bULL;
	ctx->h[7] = 0x5be0cd19137e2179ULL;
}

/**
 * sha512_copy - Copy SHA512 context
 */
static void sha512_copy(struct sha512_ctx *dst, struct sha512_ctx *src)
{
	memcpy(dst, src, sizeof(*dst));
}

/* 232 times the cube root of the first 64 primes 2..311 */
static const uint64_t k[] = {
	0x428a2f98d728ae22ULL, 0x7137449123ef65cdULL, 0xb5c0fbcfec4d3b2fULL,
	0xe9b5dba58189dbbcULL, 0x3956c25bf348b538ULL, 0x59f111f1b605d019ULL,
	0x923f82a4af194f9bULL, 0xab1c5ed5da6d8118ULL, 0xd807aa98a3030242ULL,
	0x12835b0145706fbeULL, 0x243185be4ee4b28cULL, 0x550c7dc3d5ffb4e2ULL,
	0x72be5d74f27b896fULL, 0x80deb1fe3b1696b1ULL, 0x9bdc06a725c71235ULL,
	0xc19bf174cf692694ULL, 0xe49b69c19ef14ad2ULL, 0xefbe4786384f25e3ULL,
	0x0fc19dc68b8cd5b5ULL, 0x240ca1cc77ac9c65ULL, 0x2de92c6f592b0275ULL,
	0x4a7484aa6ea6e483ULL, 0x5cb0a9dcbd41fbd4ULL, 0x76f988da831153b5ULL,
	0x983e5152ee66dfabULL, 0xa831c66d2db43210ULL, 0xb00327c898fb213fULL,
	0xbf597fc7beef0ee4ULL, 0xc6e00bf33da88fc2ULL, 0xd5a79147930aa725ULL,
	0x06ca6351e003826fULL, 0x142929670a0e6e70ULL, 0x27b70a8546d22ffcULL,
	0x2e1b21385c26c926ULL, 0x4d2c6dfc5ac42aedULL, 0x53380d139d95b3dfULL,
	0x650a73548baf63deULL, 0x766a0abb3c77b2a8ULL, 0x81c2c92e47edaee6ULL,
	0x92722c851482353bULL, 0xa2bfe8a14cf10364ULL, 0xa81a664bbc423001ULL,
	0xc24b8b70d0f89791ULL, 0xc76c51a30654be30ULL, 0xd192e819d6ef5218ULL,
	0xd69906245565a910ULL, 0xf40e35855771202aULL, 0x106aa07032bbd1b8ULL,
	0x19a4c116b8d2d0c8ULL, 0x1e376c085141ab53ULL, 0x2748774cdf8eeb99ULL,
	0x34b0bcb5e19b48a8ULL, 0x391c0cb3c5c95a63ULL, 0x4ed8aa4ae3418acbULL,
	0x5b9cca4f7763e373ULL, 0x682e6ff3d6b2b8a3ULL, 0x748f82ee5defb2fcULL,
	0x78a5636f43172f60ULL, 0x84c87814a1f0ab72ULL, 0x8cc702081a6439ecULL,
	0x90befffa23631e28ULL, 0xa4506cebde82bde9ULL, 0xbef9a3f7b2c67915ULL,
	0xc67178f2e372532bULL, 0xca273eceea26619cULL, 0xd186b8c721c0c207ULL,
	0xeada7dd6cde0eb1eULL, 0xf57d4f7fee6ed178ULL, 0x06f067aa72176fbaULL,
	0x0a637dc5a2c898a6ULL, 0x113f9804bef90daeULL, 0x1b710b35131c471bULL,
	0x28db77f523047d84ULL, 0x32caab7b40c72493ULL, 0x3c9ebe0a15c9bebcULL,
	0x431d67c49c100d4cULL, 0x4cc5d4becb3e42b6ULL, 0x597f299cfc657e2aULL,
	0x5fcb6fab3ad6faecULL, 0x6c44198c4a475817ULL,
};

static inline uint64_t Ch(uint64_t x, uint64_t y, uint64_t z)
{
	return z ^ (x & (y ^ z));
}

static inline uint64_t Maj(uint64_t x, uint64_t y, uint64_t z)
{
	return (x & y) | (z & (x | y));
}

#define e0(x)       (ror64(x, 28) ^ ror64(x, 34) ^ ror64(x, 39))
#define e1(x)       (ror64(x, 14) ^ ror64(x, 18) ^ ror64(x, 41))
#define s0(x)       (ror64(x, 1) ^ ror64(x, 8) ^ (x >> 7))
#define s1(x)       (ror64(x, 19) ^ ror64(x, 61) ^ (x >> 6))

/**
 * sha512_do_chunk - Process a block through SHA512
 */
static void sha512_do_chunk(unsigned char __W[], uint64_t H[])
{
	uint64_t a, b, c, d, e, f, g, h, t1, t2;
	uint64_t W[80];
	int i;

	for (i = 0; i < 16; i++)
		W[i] = be64_to_cpu(((uint64_t *) __W)[i]);

	for (i = 16; i < 80; i++)
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
	PASS(64);
	PASS(72);

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
 * sha512_update - Update the SHA512 context values with length bytes of data
 */
static void sha512_update(struct sha512_ctx *ctx, unsigned char *data, int len)
{
	unsigned int index, to_fill;

	/* check for partial buffer */
	index = (unsigned int) (ctx->sz[0] & 0x7f);
	to_fill = 128 - index;

	ctx->sz[0] += len;
	if (ctx->sz[0] < len)
		ctx->sz[1]++;

	/* process partial buffer if there's enough data to make a block */
	if (index && len >= to_fill) {
		memcpy(ctx->buf + index, data, to_fill);
		sha512_do_chunk(ctx->buf, ctx->h);
		len -= to_fill;
		data += to_fill;
		index = 0;
	}

	/* process as much 128-block as possible */
	for (; len >= 128; len -= 128, data += 128)
		sha512_do_chunk(data, ctx->h);

	/* append data into buf */
	if (len)
		memcpy(ctx->buf + index, data, len);
}

/**
 * sha512_finalize - Finalize the context and create the SHA512 digest
 */
static void sha512_finalize(struct sha512_ctx *ctx, sha512_digest *out)
{
	static unsigned char padding[128] = { 0x80, };
	unsigned int i, index, padlen;
	uint64_t bits[2];

	/* cpu -> big endian */
	bits[0] = cpu_to_be64((ctx->sz[1] << 3 | ctx->sz[0] >> 61));
	bits[1] = cpu_to_be64((ctx->sz[0] << 3));

	/* pad out to 56 */
	index = (unsigned int) (ctx->sz[0] & 0x7f);
	padlen = (index < 112) ? (112 - index) : ((128 + 112) - index);
	sha512_update(ctx, padding, padlen);

	/* append length */
	sha512_update(ctx, (unsigned char *) bits, sizeof(bits));

	/* store to digest */
	for (i = 0; i < 8; i++)
		out->digest[i] = cpu_to_be64(ctx->h[i]);
}

/**
 * sha512_to_bin - Transform the SHA512 digest into a binary data
 */
static void sha512_to_bin(sha512_digest *digest, char *out)
{
	uint64_t *ptr = (uint64_t *) out;
	int i;

	for (i = 0; i < 8; i++)
		ptr[i] = digest->digest[i];
}


/**
 * sha512_to_hex - Transform the SHA512 digest into a readable data
 */
static void sha512_to_hex(sha512_digest *digest, char *out)
{
	char *p;
	int i;

	for (p = out, i = 0; i < 8; i++, p += 16)
		snprintf(p, 17, "%016llx",
                         (unsigned long long) be64_to_cpu(digest->digest[i]));
}

/**
 * sha512_of_bin - Transform binary data into the SHA512 digest
 */
static void sha512_of_bin(const char *in, sha512_digest *digest)
{
	memcpy(digest->digest, in, sizeof(*digest));
}

/**
 * sha512_of_hex - Transform readable data into the SHA512 digest
 */
static void sha512_of_hex(const char *in, sha512_digest *digest)
{
	if (strlen(in) != 128)
		return;
	of_hex((unsigned char *) digest->digest, in, 128);
}

#endif
