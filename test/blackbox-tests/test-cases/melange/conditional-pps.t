Test that `(melange.preprocess (pps ...))` applies only to Melange compilation.

The field is available starting in Dune 3.24.

  $ mkdir old-preprocess
  $ cat > old-preprocess/dune-project <<EOF
  > (lang dune 3.23)
  > (using melange 0.1)
  > EOF
  $ cat > old-preprocess/dune <<EOF
  > (library
  >  (name old_preprocess)
  >  (modes melange)
  >  (melange.preprocess (pps melange.ppx)))
  > EOF
  $ dune build --root old-preprocess
  Entering directory 'old-preprocess'
  File "dune", line 4, characters 1-39:
  4 |  (melange.preprocess (pps melange.ppx)))
       ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
  Error: 'melange.preprocess' is only available since version 3.24 of the dune
  language. Please update your dune-project file to have (lang dune 3.24).
  Leaving directory 'old-preprocess'
  [1]

  $ mkdir app
  $ cat > app/dune-project <<EOF
  > (lang dune 3.24)
  > (using melange 0.1)
  > EOF
  $ cat > app/dune <<EOF
  > (library
  >  (name app)
  >  (modes melange :standard)
  >  (melange.preprocess (pps noop_ppx)))
  > EOF

  $ mkdir app/ppx
  $ cat > app/ppx/dune <<EOF
  > (library
  >  (name noop_ppx)
  >  (kind ppx_rewriter)
  >  (libraries ppxlib))
  > EOF
  $ cat > app/ppx/noop_ppx.ml <<EOF
  > let () = Ppxlib.Driver.register_transformation "noop_ppx"
  > EOF

  $ cat > app/foo.ml <<EOF
  > let x = "foo"
  > EOF
  $ cat > app/melange_only.melange.ml <<EOF
  > let x = "melange only"
  > EOF

  $ dune build --root app
  $ find ./app/_build/default -name '*.pp.ml' | censor | sort
  ./app/_build/default/.melange_src/foo.pp.ml
  ./app/_build/default/.melange_src/melange_only.pp.ml

`melange.preprocessor_deps` is available starting in Dune 3.24.

  $ mkdir old-preprocessor-deps
  $ cat > old-preprocessor-deps/dune-project <<EOF
  > (lang dune 3.23)
  > (using melange 0.1)
  > EOF
  $ cat > old-preprocessor-deps/dune <<EOF
  > (library
  >  (name old_preprocessor_deps)
  >  (modes melange)
  >  (melange.preprocessor_deps dep.txt))
  > EOF
  $ dune build --root old-preprocessor-deps
  Entering directory 'old-preprocessor-deps'
  File "dune", line 4, characters 1-36:
  4 |  (melange.preprocessor_deps dep.txt))
       ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
  Error: 'melange.preprocessor_deps' is only available since version 3.24 of
  the dune language. Please update your dune-project file to have (lang dune
  3.24).
  Leaving directory 'old-preprocessor-deps'
  [1]

`melange.preprocessor_deps` applies only to Melange preprocessing.

  $ mkdir deps
  $ cat > deps/dune-project <<EOF
  > (lang dune 3.24)
  > (using melange 0.1)
  > EOF
  $ cat > deps/dune <<EOF
  > (library
  >  (modes melange :standard)
  >  (name deps)
  >  (melange.preprocess (action (run ./pp.sh %{input-file})))
  >  (melange.preprocessor_deps pp.sh))
  > EOF

  $ cat > deps/foo.ml <<EOF
  > let x = "foo"
  > EOF
  $ cat > deps/pp.sh <<EOF
  > #!/bin/sh
  > cat "\$1"
  > echo 'let y = "from melange preprocessor"'
  > EOF
  $ chmod +x deps/pp.sh

  $ dune build --root deps
  $ grep y deps/_build/default/.melange_src/foo.pp.ml
  let y = "from melange preprocessor"

Test that paths in `node_modules` are correct for sub-libraries of the
form `foo.bar.baz`

  $ mkdir a
  $ cat > a/dune-project <<EOF
  > (lang dune 3.24)
  > (package (name a))
  > (using melange 0.1)
  > EOF
  $ cat > a/dune <<EOF
  > (library
  >  (modes melange :standard)
  >  (name a)
  >  (melange.preprocess (pps melange.ppx))
  >  (public_name a.sub))
  > EOF

  $ cat > a/foo.ml <<EOF
  > let x = "foo"
  > EOF
  $ cat > a/melange_only.melange.ml <<EOF
  > external x : unit -> < > Js.t = "" [@@mel.obj]
  > let x = x ()
  > EOF

  $ dune build --root a
  $ find ./a/_build/default -type f | censor | sort
  ./a/_build/default/.a.objs/byte/a.cmi
  ./a/_build/default/.a.objs/byte/a.cmo
  ./a/_build/default/.a.objs/byte/a.cmt
  ./a/_build/default/.a.objs/byte/a__Foo.cmi
  ./a/_build/default/.a.objs/byte/a__Foo.cmo
  ./a/_build/default/.a.objs/byte/a__Foo.cmt
  ./a/_build/default/.a.objs/melange/a.cmi
  ./a/_build/default/.a.objs/melange/a.cmj
  ./a/_build/default/.a.objs/melange/a.cmt
  ./a/_build/default/.a.objs/melange/a__Foo.cmi
  ./a/_build/default/.a.objs/melange/a__Foo.cmj
  ./a/_build/default/.a.objs/melange/a__Foo.cmt
  ./a/_build/default/.a.objs/melange/a__Melange_only.cmi
  ./a/_build/default/.a.objs/melange/a__Melange_only.cmj
  ./a/_build/default/.a.objs/melange/a__Melange_only.cmt
  ./a/_build/default/.a.objs/native/a.cmx
  ./a/_build/default/.a.objs/native/a.o
  ./a/_build/default/.a.objs/native/a__Foo.cmx
  ./a/_build/default/.a.objs/native/a__Foo.o
  ./a/_build/default/.dune/configurator
  ./a/_build/default/.dune/configurator.v2
  ./a/_build/default/.melange_src/a.ml-gen
  ./a/_build/default/.melange_src/foo.ml
  ./a/_build/default/.melange_src/foo.pp.ml
  ./a/_build/default/.melange_src/melange_only.ml
  ./a/_build/default/.melange_src/melange_only.pp.ml
  ./a/_build/default/.merlin-conf/lib-a.sub
  ./a/_build/default/.ppx/$DIGEST/_ppx.ml-gen
  ./a/_build/default/.ppx/$DIGEST/dune__exe___ppx.cmi
  ./a/_build/default/.ppx/$DIGEST/dune__exe___ppx.cmo
  ./a/_build/default/.ppx/$DIGEST/dune__exe___ppx.cmx
  ./a/_build/default/.ppx/$DIGEST/dune__exe___ppx.o
  ./a/_build/default/.ppx/$DIGEST/ppx.exe
  ./a/_build/default/META.a
  ./a/_build/default/a.a
  ./a/_build/default/a.cma
  ./a/_build/default/a.cmxa
  ./a/_build/default/a.cmxs
  ./a/_build/default/a.dune-package
  ./a/_build/default/a.install
  ./a/_build/default/a.ml-gen
  ./a/_build/default/foo.ml
  ./a/_build/default/melange_only.melange.ml
