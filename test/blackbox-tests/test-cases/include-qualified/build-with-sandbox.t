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

Transitive deps file includes the alias module

  $ cat _build/default/lib/.foo.objs/foo__Bar.impl.d
  lib/bar.ml: Sub
  $ cat _build/default/lib/.foo.objs/foo__Bar.impl.all-deps
  foo__Sub
  foo__Sub__Hello
