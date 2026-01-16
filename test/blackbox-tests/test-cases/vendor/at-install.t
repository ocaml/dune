Test that vendor stanza has the same install semantics as vendored_dirs:
vendored packages are NOT installed.

  $ cat >dune-project <<EOF
  > (lang dune 3.22)
  > (package (name myapp) (allow_empty))
  > EOF

Create a vendored library:

  $ mkdir -p duniverse/mylib.1.0.0
  $ cat >duniverse/mylib.1.0.0/dune-project <<EOF
  > (lang dune 3.0)
  > (package (name mylib))
  > EOF

  $ cat >duniverse/mylib.1.0.0/dune <<EOF
  > (library
  >  (name mylib)
  >  (public_name mylib))
  > EOF

  $ cat >duniverse/mylib.1.0.0/mylib.ml <<EOF
  > let version = "1.0.0"
  > EOF

Create main app that uses the vendored library:

  $ cat >dune <<EOF
  > (executable
  >  (name main)
  >  (libraries mylib))
  > EOF

  $ cat >main.ml <<EOF
  > let () = print_endline Mylib.version
  > EOF

Test 1: vendored_dirs - vendored packages are NOT installed
----------------------------------------------------------------------

  $ cat >duniverse/dune <<EOF
  > (vendored_dirs *)
  > EOF

  $ dune build @install

Only myapp.install exists, not mylib.install:

  $ ls _build/default/*.install
  _build/default/myapp.install

The vendored library is still usable for building:

  $ dune exec ./main.exe
  1.0.0

Test 2: vendor stanza - same semantics, vendored packages are NOT installed
----------------------------------------------------------------------

  $ cat >duniverse/dune <<EOF
  > (vendor mylib.1.0.0)
  > EOF

  $ dune clean
  $ dune build @install

Same as vendored_dirs: only myapp.install exists:

  $ ls _build/default/*.install
  _build/default/myapp.install

The vendored library is still usable:

  $ dune exec ./main.exe
  1.0.0

Test 3: vendor stanza with aliasing - library exposed under different name
----------------------------------------------------------------------

  $ cat >duniverse/dune <<EOF
  > (vendor mylib.1.0.0 (libraries (mylib :as vendored_mylib)))
  > EOF

Update main.ml to use the aliased name:

  $ cat >dune <<EOF
  > (executable
  >  (name main)
  >  (libraries vendored_mylib))
  > EOF

  $ cat >main.ml <<EOF
  > let () = print_endline Vendored_mylib.Mylib.version
  > EOF

  $ dune clean
  $ dune build @install

Aliased libraries are workspace-local, vendored packages are not installed:

  $ ls _build/default/*.install
  _build/default/myapp.install

The library works under its aliased name:

  $ dune exec ./main.exe
  1.0.0

Test 4: aliasing with private != public name
----------------------------------------------------------------------

Create a library where private name (mylib_impl) differs from public name (otherlib):

  $ mkdir -p duniverse/otherlib.1.0.0
  $ cat >duniverse/otherlib.1.0.0/dune-project <<EOF
  > (lang dune 3.0)
  > (package (name otherlib))
  > EOF

  $ cat >duniverse/otherlib.1.0.0/dune <<EOF
  > (library
  >  (name mylib_impl)
  >  (public_name otherlib)
  >  (foreign_stubs (language c) (names stub)))
  > EOF

  $ cat >duniverse/otherlib.1.0.0/mylib_impl.ml <<EOF
  > external get_version : unit -> string = "get_version"
  > let version = get_version ()
  > EOF

  $ cat >duniverse/otherlib.1.0.0/stub.c <<EOF
  > #include <caml/mlvalues.h>
  > #include <caml/alloc.h>
  > CAMLprim value get_version(value unit) {
  >   return caml_copy_string("2.0.0");
  > }
  > EOF

  $ cat >duniverse/dune <<EOF
  > (vendor otherlib.1.0.0 (libraries (otherlib :as aliased_lib)))
  > EOF

  $ cat >dune <<EOF
  > (executable
  >  (name main)
  >  (libraries aliased_lib))
  > EOF

  $ cat >main.ml <<EOF
  > let () = print_endline Aliased_lib.Mylib_impl.version
  > EOF

  $ dune clean
  $ dune build @install

  $ ls _build/default/*.install
  _build/default/myapp.install

  $ dune exec ./main.exe
  2.0.0
