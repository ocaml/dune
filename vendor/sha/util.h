#ifndef UTIL_H
#define UTIL_H

static int hex_to_int(char c)
{
    if ('0' <= c && c <= '9')
        return c - '0';
    else if ('a' <= c && c <= 'f')
        return c - 'a' + 10;
    else if ('A' <= c && c <= 'F')
        return c - 'A' + 10;
    else
        return -1;
}

static int of_hex(unsigned char *dst, const char *src, int n)
{
    int i;

    if (n % 2 != 0)
        return -1;
    for (i = 0; i < n/2; i++) {
        int a, b;
        a = hex_to_int(src[i*2]);
        if(a < 0)
            return -1;
        b = hex_to_int(src[i*2 + 1]);
        if(b < 0)
            return -1;
        dst[i] = a*16 + b;
    }
    return n/2;
}

#endif
