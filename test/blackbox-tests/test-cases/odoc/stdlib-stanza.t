Demonstrate how odoc interops with the `stdlib` stanza

  $ cat > dune-project << EOF
  > (lang dune 3.9)
  > (using experimental_building_ocaml_compiler_with_dune 0.1)
  > (package (name l))
  > EOF

  $ cat > dune << EOF
  > (library
  >  (public_name l)
  >  (stdlib))
  > EOF

  $ cat > l.ml << EOF
  > module Bar = Bar
  > (** this module is bar *)
  > EOF

  $ cat > bar.ml << EOF
  > type 'a t = 'a array
  > EOF

  $ dune build @doc
  File "_odoc/l/l/l.odoc":
  Warning: Couldn't find the following modules:
    Bar

Bar is compiled
  $ find _build/default/_doc/_odoc/l -name '*.odoc' | sort -n
  _build/default/_doc/_odoc/l/l/l.odoc
  _build/default/_doc/_odoc/l/l/l__Bar.odoc
  _build/default/_doc/_odoc/l/l/page-index.odoc
  _build/default/_doc/_odoc/l/page-index.odoc

Bar is not linked
  $ find _build/default/_doc/_odocl/l -name '*.odocl' | sort -n
  _build/default/_doc/_odocl/l/l/l.odocl
  _build/default/_doc/_odocl/l/l/page-index.odocl
  _build/default/_doc/_odocl/l/page-index.odocl

No html is generated for Bar

  $ find _build/default/_doc/_html/l -name '*.html' | sort -n
  _build/default/_doc/_html/l/index.html
  _build/default/_doc/_html/l/l/L/index.html
  _build/default/_doc/_html/l/l/index.html
