Test installation when multiple contexts are defined

  $ cat > dune-workspace <<EOF
  > (lang dune 1.11)
  > (context (default (name a)))
  > (context (default (name b)))
  > EOF

  $ cat > dune-project <<EOF
  > (lang dune 1.11)
  > (package (name a))
  > EOF

  $ cat > dune <<EOF
  > (rule (with-stdout-to hello (echo "Hello, world!")))
  > (install (section share) (files hello))
  > EOF

  $ dune build @install

Cannot install into a specific prefix with multiple contexts defined:

  $ dune install --prefix _install
  Error: Cannot specify '--prefix' or '--libdir' when installing into multiple
  contexts!
  [1]

One must pass a --context argument:

  $ dune install --prefix _install/a --context a --display short
  Installing _install/a/lib/a/META
  Installing _install/a/lib/a/dune-package
  Installing _install/a/share/a/hello

  $ dune install --prefix _install/b --context b --display short
  Installing _install/b/lib/a/META
  Installing _install/b/lib/a/dune-package
  Installing _install/b/share/a/hello

Passing an invalid context name fails:

  $ dune install --prefix _install/c --context c
  Error: Context "c" not found!
  [1]
