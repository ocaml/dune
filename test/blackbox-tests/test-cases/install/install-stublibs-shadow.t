This test shows that when libraries `libA` and `libB` containing both C stubs
are installed, and a bytecode executable `exeA` that depends on a local version
of `libA` and the installed version of `libB` is being built, then the C stubs
from the installed `libA` (on which `exeA` does not depend) are put in the load
path and take precendence over the local copy of `libA`.

See https://github.com/ocaml/dune/issues/9979.

Begin by installing libraries `libA` and `libB` with C stubs.

  $ cat >dune-project <<EOF
  > (lang dune 3.13)
  > (package (name pkgA))
  > EOF
  $ cat >libA.ml <<EOF
  > EOF
  $ cat >libB.ml <<EOF
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
         The external function `dummy2' is not available
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
