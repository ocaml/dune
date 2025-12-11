Test that -p mode isolates package documentation builds.

When building docs with -p <pkg>, we should not attempt to build docs for
other packages, even if they have unsatisfiable dependencies.

  $ cat > dune-project << EOF
  > (lang dune 3.0)
  > (package (name good-pkg))
  > (package (name bad-pkg))
  > EOF

Create a good library with satisfiable dependencies:

  $ mkdir good-lib
  $ cat > good-lib/dune << EOF
  > (library
  >  (name good_lib)
  >  (public_name good-pkg.lib))
  > EOF

  $ cat > good-lib/good_lib.ml << EOF
  > (** A good library *)
  > let hello () = "hello"
  > EOF

Create a bad library with an unsatisfiable dependency:

  $ mkdir bad-lib
  $ cat > bad-lib/dune << EOF
  > (library
  >  (name bad_lib)
  >  (public_name bad-pkg.lib)
  >  (libraries this_library_does_not_exist))
  > EOF

  $ cat > bad-lib/bad_lib.ml << EOF
  > (** A bad library *)
  > let broken () = "broken"
  > EOF

Building docs for the good package should work, even though bad-pkg has
an unsatisfiable dependency:

  $ dune build -p good-pkg @doc
  $ ls _build/default/_doc/_html/good-pkg/good-pkg.lib/Good_lib/index.html
  _build/default/_doc/_html/good-pkg/good-pkg.lib/Good_lib/index.html

Building docs for the bad package should fail:

  $ dune build -p bad-pkg @doc 2>&1 | head -5
  File "bad-lib/dune", line 4, characters 12-39:
  4 |  (libraries this_library_does_not_exist))
                  ^^^^^^^^^^^^^^^^^^^^^^^^^^^
  Error: Library "this_library_does_not_exist" not found.
  -> required by library "bad-pkg.lib" in _build/default/bad-lib
