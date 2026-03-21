Regression test: libraries with dynamic enabled_if should not cause a dependency
cycle when a (package ...) directive is present in dune-project.

Private library:

  $ cat > dune-project << EOF
  > (lang dune 3.21)
  > (package (name repro) (allow_empty))
  > EOF

  $ mkdir -p optional_lib/configure

  $ cat > optional_lib/dune << EOF
  > (library
  >  (name optional_lib)
  >  (enabled_if (= %{read:configure/enabled} "true")))
  > EOF

  $ cat > optional_lib/configure/dune << EOF
  > (rule
  >  (with-stdout-to enabled (echo "true")))
  > EOF

  $ dune build

Public library:

  $ rm -rf optional_lib

  $ cat > dune-project << EOF
  > (lang dune 3.21)
  > (package (name repro))
  > EOF

  $ mkdir -p mylib/configure

  $ cat > mylib/dune << EOF
  > (library
  >  (name mylib)
  >  (public_name repro.mylib)
  >  (enabled_if (= %{read:configure/enabled} "true")))
  > EOF

  $ cat > mylib/configure/dune << EOF
  > (rule
  >  (with-stdout-to enabled (echo "true")))
  > EOF

  $ dune build
