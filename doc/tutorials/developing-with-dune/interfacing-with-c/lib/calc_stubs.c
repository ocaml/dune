#include <caml/alloc.h>
#include <caml/memory.h>
#include <math.h>

value calc_log10 (value vx)
{
    CAMLparam1(vx);
    double x = Double_val(vx);
    double r = log10(x);
    CAMLreturn(caml_copy_double(r));
}
