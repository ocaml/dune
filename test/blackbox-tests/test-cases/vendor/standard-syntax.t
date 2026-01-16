Test the ordered set language syntax in vendor stanzas (:standard, \ exclusion, :as aliasing).

  $ cat >dune-project <<EOF
  > (lang dune 3.22)
  > (package (name myapp))
  > EOF

Create vendored library with multiple public libraries:

  $ mkdir -p duniverse/mylib.1.0.0
  $ cat >duniverse/mylib.1.0.0/dune-project <<EOF
  > (lang dune 3.0)
  > (package (name mylib))
  > EOF

  $ cat >duniverse/mylib.1.0.0/dune <<EOF
  > (library
  >  (name mylib)
  >  (modules mylib)
  >  (public_name mylib))
  > (library
  >  (name mylib_internal)
  >  (modules mylib_internal)
  >  (public_name mylib.internal))
  > (library
  >  (name mylib_extra)
  >  (modules mylib_extra)
  >  (public_name mylib.extra))
  > EOF

  $ cat >duniverse/mylib.1.0.0/mylib.ml <<EOF
  > let msg = "mylib"
  > EOF

  $ cat >duniverse/mylib.1.0.0/mylib_internal.ml <<EOF
  > let msg = "internal"
  > EOF

  $ cat >duniverse/mylib.1.0.0/mylib_extra.ml <<EOF
  > let msg = "extra"
  > EOF

Test with no libraries field (defaults to :standard):

  $ cat >duniverse/dune <<EOF
  > (vendor mylib.1.0.0)
  > EOF

  $ cat >dune <<EOF
  > (executable
  >  (name main)
  >  (libraries mylib mylib.internal mylib.extra))
  > EOF

  $ cat >main.ml <<EOF
  > let () = print_endline (Mylib.msg ^ Mylib_internal.msg ^ Mylib_extra.msg)
  > EOF

  $ dune build main.exe

Test explicit library list:

  $ cat >duniverse/dune <<EOF
  > (vendor mylib.1.0.0 (libraries mylib mylib.extra))
  > EOF

  $ cat >dune <<EOF
  > (executable
  >  (name main)
  >  (libraries mylib mylib.extra))
  > EOF

  $ cat >main.ml <<EOF
  > let () = print_endline (Mylib.msg ^ Mylib_extra.msg)
  > EOF

  $ dune build main.exe

Test :standard \ exclusion syntax - exclude mylib.internal:

  $ cat >duniverse/dune <<EOF
  > (vendor mylib.1.0.0 (libraries :standard \ mylib.internal))
  > EOF

  $ cat >dune <<EOF
  > (executable
  >  (name main)
  >  (libraries mylib mylib.extra))
  > EOF

  $ cat >main.ml <<EOF
  > let () = print_endline (Mylib.msg ^ Mylib_extra.msg)
  > EOF

  $ dune clean
  $ dune build main.exe

Verify excluded library is inaccessible:

  $ cat >dune <<EOF
  > (executable
  >  (name main)
  >  (libraries mylib.internal))
  > EOF

  $ dune build main.exe 2>&1 | head -5
  File "dune", line 3, characters 12-26:
  3 |  (libraries mylib.internal))
                  ^^^^^^^^^^^^^^
  Error: Library "mylib.internal" not found.
  -> required by _build/default/.main.eobjs/native/dune__exe__Main.cmx
  [1]

Test library aliasing with :as:

  $ cat >duniverse/dune <<EOF
  > (vendor mylib.1.0.0 (libraries (mylib :as mylib_alias)))
  > EOF

  $ cat >dune <<EOF
  > (executable
  >  (name main)
  >  (libraries mylib_alias))
  > EOF

  $ cat >main.ml <<EOF
  > let () = print_endline Mylib_alias.Mylib.msg
  > EOF

  $ dune clean
  $ dune exec ./main.exe
  mylib

Verify original name is inaccessible after aliasing:

  $ cat >dune <<EOF
  > (executable
  >  (name main)
  >  (libraries mylib))
  > EOF

  $ cat >main.ml <<EOF
  > let () = print_endline Mylib.msg
  > EOF

  $ dune build main.exe 2>&1 | head -5
  File "dune", line 3, characters 12-17:
  3 |  (libraries mylib))
                  ^^^^^
  Error: Library "mylib" not found.
  -> required by _build/default/.main.eobjs/native/dune__exe__Main.cmx
  [1]
