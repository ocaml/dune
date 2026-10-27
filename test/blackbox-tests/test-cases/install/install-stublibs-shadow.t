Bytecode linking can pick C stubs from an unrelated installed or local library
when a shared search directory contains a DLL with the same name. Changing the
order of local and installed directories can expose either collision.

See https://github.com/ocaml/dune/issues/9979.

Begin by installing libraries `libA` and `libB` with C stubs.

  $ make_dune_project_with_package 3.13 pkgA
  $ cat >libA.ml <<EOF
  > EOF
  $ cat >libB.ml <<EOF
  > external f : int -> int = "dummy1"
  > EOF
  $ cat >dune <<EOF
  > (library
  >  (name libA)
  >  (public_name pkgA.libA)
  >  (foreign_stubs
  >   (language c)
  >   (names stubsA))
  >  (modules libA))
  > (library
  >  (name libB)
  >  (public_name pkgA.libB)
  >  (foreign_stubs
  >   (language c)
  >   (names stubsB))
  >  (modules libB))
  > EOF
  $ cat >stubsA.c <<EOF
  > int dummy0() { return 0; }
  > EOF
  $ cat >stubsB.c <<EOF
  > int dummy1() { return 0; }
  > EOF
  $ dune build
  $ dune install --prefix ./install --display short 2>&1 | grep \\.so
  Installing install/lib/stublibs/dlllibA_stubs.so
  Installing install/lib/stublibs/dlllibB_stubs.so

And we try to build an executable depending on the (local) `libA` and the (installed) `libB`:

  $ mkdir -p libA
  $ cat >libA/libA.ml <<EOF
  > external f : int -> int = "dummy2"
  > EOF
  $ cat >libA/stubs.c <<EOF
  > int dummy2() { return 0; }
  > EOF
  $ cat >exeA.ml <<EOF
  > let _ = LibA.f
  > EOF
  $ cat >libA/dune <<EOF
  > (library
  >  (name libA)
  >  (foreign_stubs
  >   (language c)
  >   (names stubs))
  >  (modules libA))
  > EOF
  $ cat >dune <<EOF
  > (executable
  >  (name exeA)
  >  (modes byte)
  >  (libraries pkgA.libB libA)
  >  (modules exeA))
  > EOF

  $ OCAMLPATH=./install/lib dune build exeA.bc
  File "_none_", line 1:
  Error: Error while linking .exeA.eobjs/byte/dune__exe__ExeA.cmo:
         The external function dummy2 is not available
  [1]

The error comes from passing as flags `-I ./install/lib/pkgA/../stublibs -I
libA` so the `dlllibA_stubs.so` in `./install/lib/stublibs` takes precedence
over the one in `libA`.

If we list `libA` __before__ `pkgA.libB`, the build succeeds because the include
flags look like `-I libA -I ./install/lib/pkgA/../stublibs`:

  $ cat >dune <<EOF
  > (executable
  >  (name exeA)
  >  (modes byte)
  >  (libraries libA pkgA.libB)
  >  (modules exeA))
  > EOF

  $ OCAMLPATH=./install/lib dune build exeA.bc

Local directories can also contain unrelated stubs. Remove the unused installed
`libA` stubs to isolate the opposite collision, then build a private `libB` in
the same directory as local `libA`. The executable still uses installed
`pkgA.libB`, so it must not load this private library's stubs.

  $ rm install/lib/stublibs/dlllibA_stubs.so
  $ cat >libA/libB.ml <<EOF
  > let unused = ()
  > EOF
  $ cat >libA/other_stubs.c <<EOF
  > int dummy3() { return 0; }
  > EOF
  $ cat >>libA/dune <<EOF
  > (library
  >  (name libB)
  >  (modules libB)
  >  (foreign_stubs (language c) (names other_stubs)))
  > EOF
  $ OCAMLPATH=./install/lib dune build libA/dlllibB_stubs.so
  $ cat >exeA.ml <<EOF
  > let _ = LibA.f
  > let _ = LibB.f
  > EOF
  $ cat >dune <<EOF
  > (executable
  >  (name exeA)
  >  (modes byte)
  >  (libraries pkgA.libB libA)
  >  (modules exeA))
  > EOF
  $ OCAMLPATH=./install/lib dune build exeA.bc

Putting the local directory first now selects the unrelated local `libB` stubs.

  $ cat >dune <<EOF
  > (executable
  >  (name exeA)
  >  (modes byte)
  >  (libraries libA pkgA.libB)
  >  (modules exeA))
  > EOF
  $ OCAMLPATH=./install/lib dune build exeA.bc
  File "_none_", line 1:
  Error: Error while linking .exeA.eobjs/byte/dune__exe__ExeA.cmo:
         The external function dummy1 is not available
  [1]
