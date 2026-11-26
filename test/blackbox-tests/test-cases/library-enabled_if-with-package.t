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

  $ make_dune_project_with_package 3.21 repro

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

Package metadata contains only the requested package's enabled public and
packaged private libraries. Deprecated names need not resolve their destination.

  $ mkdir metadata
  $ cd metadata
  $ cat > dune-project <<EOF
  > (lang dune 3.21)
  > (package (name foo))
  > (package (name bar))
  > (package (name empty) (allow_empty))
  > EOF
  $ mkdir a b
  $ touch a/foo.ml a/secret.ml a/disabled.ml b/bar.ml
  $ cat > a/dune <<EOF
  > (library (public_name foo) (modules foo))
  > (library (name secret) (package foo) (modules secret))
  > (library
  >  (name disabled)
  >  (public_name foo.disabled)
  >  (modules disabled)
  >  (enabled_if false))
  > (deprecated_library_name
  >  (old_public_name foo.old)
  >  (new_public_name bar.missing))
  > EOF
  $ cat > b/dune <<EOF
  > (library (public_name bar))
  > EOF

Request metadata for several packages without package filtering.

  $ dune build META.foo foo.dune-package META.bar META.empty
  $ grep '^package ' _build/default/META.foo
  package "__private__" (
  package "old" (
  $ grep 'archive(byte)' _build/default/META.foo
  archive(byte) = "foo.cma"
      archive(byte) = "secret.cma"
  $ grep 'bar.missing' _build/default/META.foo
    requires = "bar.missing"
    exports = "bar.missing"
  $ grep 'disabled' _build/default/META.foo
  [1]
  $ grep 'archive(byte)' _build/default/META.bar
  archive(byte) = "bar.cma"
  $ grep 'archive(byte)' _build/default/META.empty
  [1]
