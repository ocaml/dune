Libraries with (archived false) compile modules without producing archives.

  $ mkdir all
  $ cat >all/dune-project <<EOF
  > (lang dune 3.23)
  > EOF
  $ cat >all/dune <<EOF
  > (library
  >  (name base)
  >  (modules base)
  >  (archived false))
  > EOF
  $ cat >all/base.ml <<EOF
  > let x = 1
  > EOF

  $ dune build --root all @all
  Entering directory 'all'
  Leaving directory 'all'
  $ find all/_build/default -name '*.cm*' | sort
  all/_build/default/.base.objs/byte/base.cmi
  all/_build/default/.base.objs/byte/base.cmo
  all/_build/default/.base.objs/byte/base.cmt
  all/_build/default/.base.objs/native/base.cmx

  $ mkdir ok
  $ cat >ok/dune-project <<EOF
  > (lang dune 3.23)
  > EOF
  $ cat >ok/dune <<EOF
  > (library
  >  (name base)
  >  (modules base)
  >  (archived false))
  > (library
  >  (name middle)
  >  (modules middle)
  >  (libraries base))
  > (executable
  >  (name main)
  >  (modules main)
  >  (libraries middle))
  > EOF
  $ cat >ok/base.ml <<EOF
  > let message = "base"
  > EOF
  $ cat >ok/middle.ml <<EOF
  > let message = Base.message ^ "+middle"
  > EOF
  $ cat >ok/main.ml <<EOF
  > print_endline Middle.message
  > EOF

  $ dune exec --root ok ./main.exe
  Entering directory 'ok'
  Leaving directory 'ok'
  base+middle
  $ dune build --root ok @all
  Entering directory 'ok'
  Leaving directory 'ok'
  $ find ok/_build/default -name '*.cm*a' | sort
  ok/_build/default/middle.cma
  ok/_build/default/middle.cmxa

(archived false) libraries must remain private.

  $ mkdir public
  $ cat >public/dune-project <<EOF
  > (lang dune 3.23)
  > (package
  >  (name foo))
  > EOF
  $ cat >public/dune <<EOF
  > (library
  >  (name base)
  >  (public_name foo.base)
  >  (archived false))
  > EOF
  $ cat >public/base.ml <<EOF
  > let x = 1
  > EOF

  $ dune build --root public
  Entering directory 'public'
  File "dune", line 3, characters 14-22:
  3 |  (public_name foo.base)
                    ^^^^^^^^
  Error: (archived false) libraries must be private.
  Leaving directory 'public'
  [1]

(archived false) libraries may not be attached to a package.

  $ mkdir package
  $ cat >package/dune-project <<EOF
  > (lang dune 3.23)
  > (package
  >  (name foo))
  > EOF
  $ cat >package/dune <<EOF
  > (library
  >  (name base)
  >  (package foo)
  >  (archived false))
  > EOF
  $ cat >package/base.ml <<EOF
  > let x = 1
  > EOF

  $ dune build --root package
  Entering directory 'package'
  File "dune-project", lines 2-3, characters 0-21:
  2 | (package
  3 |  (name foo))
  Error: (archived false) libraries may not be attached to a package.
  Leaving directory 'package'
  [1]

(archived false) libraries may not use foreign stubs.

  $ mkdir foreign
  $ cat >foreign/dune-project <<EOF
  > (lang dune 3.23)
  > EOF
  $ cat >foreign/dune <<EOF
  > (library
  >  (name base)
  >  (archived false)
  >  (foreign_stubs
  >   (language c)
  >   (names stub)))
  > EOF
  $ cat >foreign/base.ml <<EOF
  > let x = 1
  > EOF

  $ dune build --root foreign
  Entering directory 'foreign'
  File "dune", lines 1-6, characters 0-87:
  1 | (library
  2 |  (name base)
  3 |  (archived false)
  4 |  (foreign_stubs
  5 |   (language c)
  6 |   (names stub)))
  Error: (archived false) libraries may not use foreign stubs, foreign
  archives, extra objects, or ctypes.
  Leaving directory 'foreign'
  [1]

(archived false) may not be used on virtual libraries.

  $ mkdir virtual
  $ cat >virtual/dune-project <<EOF
  > (lang dune 3.23)
  > EOF
  $ cat >virtual/dune <<EOF
  > (library
  >  (name vlib)
  >  (virtual_modules vlib)
  >  (archived false))
  > EOF
  $ cat >virtual/vlib.mli <<EOF
  > val x : int
  > EOF

  $ dune build --root virtual
  Entering directory 'virtual'
  File "dune", line 3, characters 18-22:
  3 |  (virtual_modules vlib)
                        ^^^^
  Error: (archived false) may not be used on virtual libraries.
  Leaving directory 'virtual'
  [1]
