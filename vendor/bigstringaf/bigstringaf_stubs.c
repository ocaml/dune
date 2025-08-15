/*----------------------------------------------------------------------------
    Copyright (c) 2017 Inhabited Type LLC.

    All rights reserved.

    Redistribution and use in source and binary forms, with or without
    modification, are permitted provided that the following conditions
    are met:

    1. Redistributions of source code must retain the above copyright
       notice, this list of conditions and the following disclaimer.

    2. Redistributions in binary form must reproduce the above copyright
       notice, this list of conditions and the following disclaimer in the
       documentation and/or other materials provided with the distribution.

    3. Neither the name of the author nor the names of his contributors
       may be used to endorse or promote products derived from this software
       without specific prior written permission.

    THIS SOFTWARE IS PROVIDED BY THE CONTRIBUTORS ``AS IS'' AND ANY EXPRESS
    OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
    WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
    DISCLAIMED.  IN NO EVENT SHALL THE AUTHORS OR CONTRIBUTORS BE LIABLE FOR
    ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
    DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS
    OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION)
    HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT,
    STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN
    ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
    POSSIBILITY OF SUCH DAMAGE.
  ----------------------------------------------------------------------------*/

#include <string.h>
#include <caml/mlvalues.h>
#include <caml/bigarray.h>

CAMLprim value
bigstringaf_blit_to_bytes(value vsrc, value vsrc_off, value vdst, value vdst_off, value vlen)
{
    void *src = ((char *)Caml_ba_data_val(vsrc)) + Unsigned_long_val(vsrc_off),
         *dst = ((char *)String_val(vdst))       + Unsigned_long_val(vdst_off);
    size_t len = Unsigned_long_val(vlen);
    memcpy(dst, src, len);
    return Val_unit;
}

CAMLprim value
bigstringaf_blit_to_bigstring(value vsrc, value vsrc_off, value vdst, value vdst_off, value vlen)
{
    void *src = ((char *)Caml_ba_data_val(vsrc)) + Unsigned_long_val(vsrc_off),
         *dst = ((char *)Caml_ba_data_val(vdst)) + Unsigned_long_val(vdst_off);
    size_t len = Unsigned_long_val(vlen);
    memmove(dst, src, len);
    return Val_unit;
}

CAMLprim value
bigstringaf_blit_from_bytes(value vsrc, value vsrc_off, value vdst, value vdst_off, value vlen)
{
    void *src = ((char *)String_val(vsrc))       + Unsigned_long_val(vsrc_off),
         *dst = ((char *)Caml_ba_data_val(vdst)) + Unsigned_long_val(vdst_off);
    size_t len = Unsigned_long_val(vlen);
    memcpy(dst, src, len);
    return Val_unit;
}

CAMLprim value
bigstringaf_memcmp_bigstring(value vba1, value vba1_off, value vba2, value vba2_off, value vlen)
{
    void *ba1 = ((char *)Caml_ba_data_val(vba1)) + Unsigned_long_val(vba1_off),
         *ba2 = ((char *)Caml_ba_data_val(vba2)) + Unsigned_long_val(vba2_off);
    size_t len = Unsigned_long_val(vlen);

    int result = memcmp(ba1, ba2, len);
    return Val_int(result);
}

CAMLprim value
bigstringaf_memcmp_string(value vba, value vba_off, value vstr, value vstr_off, value vlen)
{
    void *buf1 = ((char *)Caml_ba_data_val(vba)) + Unsigned_long_val(vba_off),
         *buf2 = ((char *)String_val(vstr))      + Unsigned_long_val(vstr_off);
    size_t len = Unsigned_long_val(vlen);

    int result = memcmp(buf1, buf2, len);
    return Val_int(result);
}

CAMLprim value
bigstringaf_memchr(value vba, value vba_off, value vchr, value vlen)
{
    size_t off = Unsigned_long_val(vba_off);
    char *buf = ((char *)Caml_ba_data_val(vba)) + off;
    size_t len = Unsigned_long_val(vlen);
    int c = Int_val(vchr);

    char* res = memchr(buf, c, len);
    if (res == NULL)
    {
        return Val_long(-1);
    }
    else
    {
        return Val_long(off + res - buf);
    }
}
