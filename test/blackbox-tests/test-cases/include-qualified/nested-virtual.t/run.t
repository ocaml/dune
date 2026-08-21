We can nested modules virtual
  $ dune build @all

The logical path is preserved when the virtual library is installed:

  $ dune install --prefix="$PWD/prefix"
  $ mkdir -p external/bar
  $ cp impl/bar/virt.ml external/bar/virt.ml
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
