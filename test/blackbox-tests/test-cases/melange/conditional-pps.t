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

  $ pp_dir=_build/default/.melange_src
  $ dune build --root app \
  >   "$pp_dir/foo.pp.ml" \
  >   "$pp_dir/melange_only.pp.ml"
  $ find "./app/$pp_dir" -name '*.pp.ml' | censor | sort
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

  $ pp=_build/default/.melange_src/foo.pp.ml
  $ dune build --root deps "$pp"
  $ grep y "deps/$pp"
  let y = "from melange preprocessor"

`melange.preprocess` can use Melange syntax in a Melange-only module of a
mixed-mode library.

  $ mkdir melange-ppx
  $ cat > melange-ppx/dune-project <<EOF
  > (lang dune 3.24)
  > (package (name melange-ppx))
  > (using melange 0.1)
  > EOF
  $ cat > melange-ppx/dune <<EOF
  > (library
  >  (modes melange :standard)
  >  (name melange_ppx)
  >  (public_name melange-ppx)
  >  (melange.preprocess (pps melange.ppx)))
  > EOF

  $ cat > melange-ppx/foo.ml <<EOF
  > let x = "foo"
  > EOF
  $ cat > melange-ppx/melange_only.melange.ml <<EOF
  > external x : unit -> < > Js.t = "" [@@mel.obj]
  > let x = x ()
  > EOF

  $ pp_dir=_build/default/.melange_src
  $ dune build --root melange-ppx \
  >   "$pp_dir/foo.pp.ml" \
  >   "$pp_dir/melange_only.pp.ml"
  $ find "melange-ppx/$pp_dir" -name '*.pp.ml' | sort
  melange-ppx/_build/default/.melange_src/foo.pp.ml
  melange-ppx/_build/default/.melange_src/melange_only.pp.ml
