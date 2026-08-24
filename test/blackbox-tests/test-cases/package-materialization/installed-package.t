Test that (deps (package ...)) works with externally installed packages.
Installed packages (found via findlib) go through the Installed codepath,
not the layout. The layout only applies to Local (workspace) packages.

Install packages "a" and "b" into a prefix. Library `a` depends on library
`b`, so its installed metadata records `b` as a requirement.

  $ mkdir a consumer prefix

  $ cat >a/dune-project <<EOF
  > (lang dune 3.24)
  > (package (name a))
  > (package (name b))
  > EOF

  $ cat >a/dune <<EOF
  > (library
  >  (public_name a)
  >  (libraries b))
  > EOF

  $ cat >a/a.ml <<EOF
  > let value = B.value + 1
  > EOF

  $ mkdir a/b

  $ cat >a/b/dune <<EOF
  > (library (public_name b))
  > EOF

  $ cat >a/b/b.ml <<EOF
  > let value = 1
  > EOF

  $ dune build --root a @install
  $ dune install --root a --prefix $PWD/prefix 2>/dev/null
  $ test -f prefix/lib/a/META
  $ test -f prefix/lib/b/META

Now create a consumer project that depends on the installed package.
The consumer uses `(deps (package a))` and external OCaml tooling to verify
that both `a` and its library dependency are findable:

  $ cat >consumer/dune-project <<EOF
  > (lang dune 3.24)
  > EOF

  $ cat >consumer/main.ml <<EOF
  > let () = print_int A.value
  > EOF

  $ cat >consumer/dune <<'EOF'
  > (rule
  >  (target main.exe)
  >  (deps
  >   main.ml
  >   (package a))
  >  (action
  >   (run ocamlfind ocamlc -package a -linkpkg -o %{target} main.ml)))
  > EOF

  $ OCAMLPATH=$PWD/prefix/lib dune build --root consumer main.exe
  $ consumer/_build/default/main.exe
  2

When `--only-packages` masks a workspace library in the closure, library
resolution falls back to its installed copy. The installed library remains on
the inherited `OCAMLPATH`; it is not rematerialized as workspace support.

  $ mkdir masked masked/a-src masked/b-src

  $ cat >masked/dune-project <<EOF
  > (lang dune 3.24)
  > (package (name a))
  > (package (name b))
  > EOF

  $ cat >masked/a-src/dune <<EOF
  > (library
  >  (public_name a)
  >  (libraries b))
  > EOF

  $ cat >masked/a-src/a.ml <<EOF
  > let value = B.value + 10
  > EOF

  $ cat >masked/b-src/dune <<EOF
  > (library (public_name b))
  > EOF

  $ cat >masked/b-src/b.ml <<EOF
  > let value = 100
  > EOF

  $ cat >masked/main.ml <<EOF
  > let () = print_int A.value
  > EOF

  $ cat >masked/dune <<'EOF'
  > (rule
  >  (target main.exe)
  >  (deps
  >   main.ml
  >   (package a))
  >  (action
  >   (run %{bin:ocamlfind} ocamlc -package a -linkpkg -o %{target} main.ml)))
  > EOF

  $ OCAMLPATH=$PWD/prefix/lib dune build --root masked --only-packages a main.exe
  $ masked/_build/default/main.exe
  11
