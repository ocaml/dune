Test foreign sources lookup with vendor stanza aliasing.

  $ cat > dune-project << EOF
  > (lang dune 3.22)
  > (package (name myapp) (allow_empty))
  > EOF

Test 1: Simple library with C stubs, aliased
----------------------------------------------------------------------

  $ mkdir -p vendor/clib.1.0.0
  $ cat > vendor/clib.1.0.0/dune-project << EOF
  > (lang dune 3.0)
  > (package (name clib))
  > EOF

  $ cat > vendor/clib.1.0.0/dune << EOF
  > (library
  >  (name clib)
  >  (public_name clib)
  >  (foreign_stubs (language c) (names stubs)))
  > EOF

  $ cat > vendor/clib.1.0.0/clib.ml << EOF
  > external add : int -> int -> int = "caml_add"
  > EOF

  $ cat > vendor/clib.1.0.0/stubs.c << EOF
  > #include <caml/mlvalues.h>
  > CAMLprim value caml_add(value a, value b) {
  >   return Val_int(Int_val(a) + Int_val(b));
  > }
  > EOF

Use vendor stanza with alias (no vendored_dirs):
  $ cat > vendor/dune << EOF
  > (vendor clib.1.0.0 (libraries (clib :as my_clib)))
  > EOF

  $ cat > dune << EOF
  > (executable
  >  (name main)
  >  (libraries my_clib))
  > EOF

  $ cat > main.ml << EOF
  > let () = Printf.printf "1 + 2 = %d\n" (My_clib.Clib.add 1 2)
  > EOF

Build and run:
  $ dune exec ./main.exe
  1 + 2 = 3

Build @install (this exercises dune-package generation):
  $ dune build @install

Test 2: Library with foreign stubs where private name differs from public name
----------------------------------------------------------------------

  $ rm -rf vendor/clib.1.0.0
  $ mkdir -p vendor/myimpl.1.0.0
  $ cat > vendor/myimpl.1.0.0/dune-project << EOF
  > (lang dune 3.0)
  > (package (name pubname))
  > EOF

  $ cat > vendor/myimpl.1.0.0/dune << EOF
  > (library
  >  (name private_impl)
  >  (public_name pubname)
  >  (foreign_stubs (language c) (names native_code)))
  > EOF

  $ cat > vendor/myimpl.1.0.0/private_impl.ml << EOF
  > external multiply : int -> int -> int = "caml_multiply"
  > EOF

  $ cat > vendor/myimpl.1.0.0/native_code.c << EOF
  > #include <caml/mlvalues.h>
  > CAMLprim value caml_multiply(value a, value b) {
  >   return Val_int(Int_val(a) * Int_val(b));
  > }
  > EOF

Alias uses the public name (pubname), not the private name (private_impl):
  $ cat > vendor/dune << EOF
  > (vendor myimpl.1.0.0 (libraries (pubname :as aliased_pub)))
  > EOF

  $ cat > dune << EOF
  > (executable
  >  (name main)
  >  (libraries aliased_pub))
  > EOF

  $ cat > main.ml << EOF
  > let () = Printf.printf "3 * 4 = %d\n" (Aliased_pub.Private_impl.multiply 3 4)
  > EOF

  $ dune clean
  $ dune exec ./main.exe
  3 * 4 = 12

  $ dune build @install

Test 3: Two vendored packages with foreign stubs, both aliased
----------------------------------------------------------------------

  $ rm -rf vendor/myimpl.1.0.0
  $ mkdir -p vendor/mathlib_a.1.0.0 vendor/mathlib_b.1.0.0

Package A:
  $ cat > vendor/mathlib_a.1.0.0/dune-project << EOF
  > (lang dune 3.0)
  > (package (name mathlib_a))
  > EOF

  $ cat > vendor/mathlib_a.1.0.0/dune << EOF
  > (library
  >  (name mathlib_a)
  >  (public_name mathlib_a)
  >  (foreign_stubs (language c) (names math_stubs)))
  > EOF

  $ cat > vendor/mathlib_a.1.0.0/mathlib_a.ml << EOF
  > external compute : int -> int = "caml_compute_a"
  > let version = "A"
  > EOF

  $ cat > vendor/mathlib_a.1.0.0/math_stubs.c << EOF
  > #include <caml/mlvalues.h>
  > CAMLprim value caml_compute_a(value x) {
  >   return Val_int(Int_val(x) + 100);
  > }
  > EOF

Package B:
  $ cat > vendor/mathlib_b.1.0.0/dune-project << EOF
  > (lang dune 3.0)
  > (package (name mathlib_b))
  > EOF

  $ cat > vendor/mathlib_b.1.0.0/dune << EOF
  > (library
  >  (name mathlib_b)
  >  (public_name mathlib_b)
  >  (foreign_stubs (language c) (names math_stubs)))
  > EOF

  $ cat > vendor/mathlib_b.1.0.0/mathlib_b.ml << EOF
  > external compute : int -> int = "caml_compute_b"
  > let version = "B"
  > EOF

  $ cat > vendor/mathlib_b.1.0.0/math_stubs.c << EOF
  > #include <caml/mlvalues.h>
  > CAMLprim value caml_compute_b(value x) {
  >   return Val_int(Int_val(x) + 200);
  > }
  > EOF

Alias each:
  $ cat > vendor/dune << EOF
  > (vendor mathlib_a.1.0.0 (libraries (mathlib_a :as math_v1)))
  > (vendor mathlib_b.1.0.0 (libraries (mathlib_b :as math_v2)))
  > EOF

  $ cat > dune << EOF
  > (executable
  >  (name main)
  >  (libraries math_v1 math_v2))
  > EOF

  $ cat > main.ml << EOF
  > let () =
  >   Printf.printf "v1: %s, compute(5) = %d\n"
  >     Math_v1.Mathlib_a.version
  >     (Math_v1.Mathlib_a.compute 5);
  >   Printf.printf "v2: %s, compute(5) = %d\n"
  >     Math_v2.Mathlib_b.version
  >     (Math_v2.Mathlib_b.compute 5)
  > EOF

  $ dune clean
  $ dune exec ./main.exe
  v1: A, compute(5) = 105
  v2: B, compute(5) = 205

  $ dune build @install

Test 4: Multiple libraries in same vendored package, each aliased
----------------------------------------------------------------------

  $ rm -rf vendor/mathlib_a.1.0.0 vendor/mathlib_b.1.0.0
  $ mkdir -p vendor/multilib.1.0.0

  $ cat > vendor/multilib.1.0.0/dune-project << EOF
  > (lang dune 3.0)
  > (package (name multilib))
  > EOF

Need explicit modules since multiple libraries in same directory:
  $ cat > vendor/multilib.1.0.0/dune << EOF
  > (library
  >  (name lib_a)
  >  (modules lib_a)
  >  (public_name multilib.a)
  >  (foreign_stubs (language c) (names stubs_a)))
  > (library
  >  (name lib_b)
  >  (modules lib_b)
  >  (public_name multilib.b)
  >  (foreign_stubs (language c) (names stubs_b)))
  > EOF

  $ cat > vendor/multilib.1.0.0/lib_a.ml << EOF
  > external value_a : unit -> int = "get_value_a"
  > EOF

  $ cat > vendor/multilib.1.0.0/lib_b.ml << EOF
  > external value_b : unit -> int = "get_value_b"
  > EOF

  $ cat > vendor/multilib.1.0.0/stubs_a.c << EOF
  > #include <caml/mlvalues.h>
  > CAMLprim value get_value_a(value unit) { return Val_int(111); }
  > EOF

  $ cat > vendor/multilib.1.0.0/stubs_b.c << EOF
  > #include <caml/mlvalues.h>
  > CAMLprim value get_value_b(value unit) { return Val_int(222); }
  > EOF

Alias both libraries from the same package:
  $ cat > vendor/dune << EOF
  > (vendor multilib.1.0.0 (libraries (multilib.a :as alias_a) (multilib.b :as alias_b)))
  > EOF

  $ cat > dune << EOF
  > (executable
  >  (name main)
  >  (libraries alias_a alias_b))
  > EOF

  $ cat > main.ml << EOF
  > let () =
  >   Printf.printf "a = %d, b = %d\n"
  >     (Alias_a.Lib_a.value_a ())
  >     (Alias_b.Lib_b.value_b ())
  > EOF

  $ dune clean
  $ dune exec ./main.exe
  a = 111, b = 222

  $ dune build @install
