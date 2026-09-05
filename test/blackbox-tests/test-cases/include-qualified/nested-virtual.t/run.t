We can make nested modules virtual
  $ mkdir -p vlib/group impl/group vlib/foo/foo impl/foo/foo
  $ cat >vlib/group/group.mli <<'EOF'
  > val value : int
  > EOF
  $ cat >impl/group/group.ml <<'EOF'
  > let value = 42
  > EOF
  $ cat >vlib/foo/foo/foo.mli <<'EOF'
  > val value : int
  > EOF
  $ cat >impl/foo/foo/foo.ml <<'EOF'
  > let value = 42
  > EOF
  $ dune build @all

The logical path is preserved when the virtual library is installed:

  $ dune install --prefix="$PWD/prefix"
  $ grep -o '(path Group Group)' prefix/lib/vlib/dune-package
  (path Group Group)
  $ grep -o '(path Foo Foo Foo)' prefix/lib/vlib/dune-package
  (path Foo Foo Foo)
  $ mkdir -p external/bar
  $ cp impl/bar/virt.ml external/bar/virt.ml
  $ mkdir external/group
  $ cp impl/group/group.ml external/group/group.ml
  $ mkdir -p external/foo/foo
  $ cp impl/foo/foo/foo.ml external/foo/foo/foo.ml
  $ cat >external/dune-project <<'EOF'
  > (lang dune 3.25)
  > EOF
  $ cat >external/dune <<'EOF'
  > (include_subdirs qualified)
  > (library
  >  (name impl)
  >  (implements vlib))
  > EOF
  $ OCAMLPATH="$PWD/prefix/lib" dune build --root external @all
