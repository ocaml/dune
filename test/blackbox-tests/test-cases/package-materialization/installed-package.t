Test that (deps (package ...)) depends on the library closure of externally
installed packages. This must work whether the installed metadata is a
`dune-package` file or a findlib META file.

Install packages "a" and "b" into a prefix. Library `a` depends on library
`b`, so both installed metadata formats record `b` as a requirement.

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

  $ mkdir -p a/b/unrelated

  $ cat >a/b/dune <<EOF
  > (library (public_name b))
  > EOF

  $ cat >a/b/b.ml <<EOF
  > let value = 1
  > EOF

  $ cat >a/b/unrelated/dune <<EOF
  > (library
  >  (name unrelated)
  >  (public_name b.unrelated))
  > EOF

  $ cat >a/b/unrelated/unrelated.ml <<EOF
  > let value = 2
  > EOF

  $ dune build --root a @install
  $ dune install --root a --prefix $PWD/prefix 2>/dev/null
  $ test -f prefix/lib/a/META
  $ test -f prefix/lib/b/META
  $ test -f prefix/lib/b/unrelated/unrelated.cmi

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

The rule must depend on the required library's files, not merely inherit an
environment in which they happen to be visible.

  $ OCAMLPATH=$PWD/prefix/lib dune rules --root consumer --format=json main.exe |
  > jq_dune '.[] | ruleDepFilePaths' |
  > grep -q "$PWD/prefix/lib/b/b.cmi"
  [1]
  $ OCAMLPATH=$PWD/prefix/lib dune rules --root consumer --format=json main.exe |
  > jq_dune '.[] | ruleDepFilePaths' |
  > grep -q "$PWD/prefix/lib/b/dune-package"
  [1]
  $ ! OCAMLPATH=$PWD/prefix/lib dune rules --root consumer --format=json main.exe |
  > jq_dune '.[] | ruleDepFilePaths' |
  > grep -q "$PWD/prefix/lib/b/unrelated/unrelated.cmi"

Remove Dune's external package metadata and repeat the same checks through the
findlib META reader.

  $ rm prefix/lib/a/dune-package prefix/lib/b/dune-package
  $ rm -rf consumer/_build
  $ OCAMLPATH=$PWD/prefix/lib dune build --root consumer main.exe
  $ consumer/_build/default/main.exe
  2
  $ OCAMLPATH=$PWD/prefix/lib dune rules --root consumer --format=json main.exe |
  > jq_dune '.[] | ruleDepFilePaths' |
  > grep -q "$PWD/prefix/lib/b/b.cmi"
  [1]
  $ OCAMLPATH=$PWD/prefix/lib dune rules --root consumer --format=json main.exe |
  > jq_dune '.[] | ruleDepFilePaths' |
  > grep -q "$PWD/prefix/lib/b/META"
  [1]
  $ ! OCAMLPATH=$PWD/prefix/lib dune rules --root consumer --format=json main.exe |
  > jq_dune '.[] | ruleDepFilePaths' |
  > grep -q "$PWD/prefix/lib/b/unrelated/unrelated.cmi"

When `--only-packages` masks a workspace library in the closure, library
resolution falls back to its installed copy. That installed library must still
be an action dependency rather than merely visible on the inherited
`OCAMLPATH`.

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
  $ OCAMLPATH=$PWD/prefix/lib dune rules --root masked --only-packages a \
  > --format=json main.exe |
  > jq_dune '.[] | ruleDepFilePaths' |
  > grep -q "$PWD/prefix/lib/b/b.cmi"
  [1]
  $ OCAMLPATH=$PWD/prefix/lib dune rules --root masked --only-packages a \
  > --format=json main.exe |
  > jq_dune '.[] | ruleDepFilePaths' |
  > grep -q "$PWD/prefix/lib/b/META"
  [1]
  $ ! OCAMLPATH=$PWD/prefix/lib dune rules --root masked --only-packages a \
  > --format=json main.exe |
  > jq_dune '.[] | ruleDepFilePaths' |
  > grep -q "$PWD/prefix/lib/b/unrelated/unrelated.cmi"
