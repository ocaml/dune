Test the remap facility for documentation generation

This test verifies that:
1. @doc builds only local packages
2. @doc-full builds all packages (local + installed)
3. Both modes work correctly

Setup a simple project with a local library:

  $ cat > dune-project <<EOF
  > (lang dune 3.17)
  > (package (name mylib))
  > EOF

  $ cat > mylib.opam <<EOF
  > EOF

  $ cat > dune <<EOF
  > (library
  >  (name mylib)
  >  (public_name mylib))
  > EOF

  $ cat > mylib.ml <<EOF
  > (** My library *)
  > let hello () = "Hello, world!"
  > EOF

Build documentation with @doc (local-only mode):

  $ dune build @doc

Check that local package HTML was built:

  $ find _build/default/_doc/_html -name "*.html" | grep -E "mylib|index" | sort
  _build/default/_doc/_html/index.html
  _build/default/_doc/_html/mylib/index.html
  _build/default/_doc/_html/mylib/mylib/Mylib/index.html
  _build/default/_doc/_html/mylib/mylib/index.html

Build documentation with @doc-full (full mode):

  $ dune build @doc-full

Check that HTML was built in _html_full for key modules:

  $ ls _build/default/_doc/_html_full/index.html
  _build/default/_doc/_html_full/index.html
  $ ls _build/default/_doc/_html_full/mylib/mylib/Mylib/index.html
  _build/default/_doc/_html_full/mylib/mylib/Mylib/index.html

Verify stdlib documentation was also built (package name varies by OCaml version):

  $ find _build/default/_doc/_html_full -path "*/stdlib/Stdlib/Option/index.html" | wc -l | tr -d ' '
  1
  $ find _build/default/_doc/_html_full -path "*/stdlib/Stdlib/List/index.html" | wc -l | tr -d ' '
  1

Verify both directory structures exist:

  $ test -d _build/default/_doc/_html && echo "_html exists"
  _html exists

  $ test -d _build/default/_doc/_html_full && echo "_html_full exists"
  _html_full exists
