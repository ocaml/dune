Test `(include_subdirs qualified)` with sandboxing

  $ mkdir lib
  $ cat > dune-project <<EOF
  > (lang dune 3.20)
  > EOF

  $ mkdir lib/sub
  $ cat > lib/dune <<EOF
  > (include_subdirs qualified)
  > (library (name foo))
  > EOF
  $ cat > lib/bar.ml <<EOF
  > let hello = Sub.Hello.hello
  > EOF
  $ cat > lib/sub/hello.ml <<EOF
  > let hello = "hello from sub"
  > EOF

  $ DUNE_SANDBOX=symlink dune build

No transitive deps file is materialized.

  $ find _build -name "*.all-deps" | sort

Transitive alias dependencies are still available under sandboxing when a
compiled interface exposes a qualified submodule path.

  $ cat > lib/a.ml <<EOF
  > let x = B.x
  > EOF
  $ cat > lib/b.mli <<EOF
  > val x : Sub.T.t
  > EOF
  $ cat > lib/b.ml <<EOF
  > let x = Sub.T.x
  > EOF
  $ cat > lib/sub/t.ml <<EOF
  > type t = int
  > let x = 0
  > EOF

  $ DUNE_SANDBOX=symlink dune build

  $ cat > lib/dune <<EOF
  > (include_subdirs qualified)
  > (library (name foo))
  > EOF
  $ cat > lib/bar.ml <<EOF
  > let hello = Sub.Hello.hello ^ Sub.world
  > EOF
  $ cat > lib/sub/sub.ml <<EOF
  > module Hello = Hello
  > module T = T
  > let world = "world"
  > EOF
  $ cat > lib/sub/hello.ml <<EOF
  > let hello = "hello from sub"
  > EOF

  $ DUNE_SANDBOX=symlink dune build

  $ find _build -name "*.all-deps" | sort
