Test that documentation works for libraries with no modules.

  $ cat > dune-project << EOF
  > (lang dune 3.0)
  > (package (name mypkg))
  > EOF

  $ cat > dune << EOF
  > (library
  >  (name empty_lib)
  >  (public_name mypkg.empty)
  >  (modules))
  > (library
  >  (name normal_lib)
  >  (public_name mypkg.normal))
  > EOF

  $ cat > normal_lib.ml << EOF
  > let x = 42
  > EOF

Build documentation:

  $ dune build @doc 2>&1 | head -20

Check that documentation was generated for both libraries:

  $ find _build/default/_doc/_html/mypkg -name 'index.html' | sort
  _build/default/_doc/_html/mypkg/index.html
  _build/default/_doc/_html/mypkg/mypkg.empty/Empty_lib/index.html
  _build/default/_doc/_html/mypkg/mypkg.empty/index.html
  _build/default/_doc/_html/mypkg/mypkg.normal/Normal_lib/index.html
  _build/default/_doc/_html/mypkg/mypkg.normal/index.html
