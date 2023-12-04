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

Bar is compiled
  $ find _build/default -name '*.odoc' | sort -n
  _build/default/.l.objs/byte/l.odoc
  _build/default/.l.objs/byte/l__Bar.odoc
  _build/default/_doc/_odoc/pkg/l/page-index.odoc

Bar is not linked
  $ find _build/default -name '*.odocl' | sort -n
  _build/default/_doc/_odocls/l/l.odocl
  _build/default/_doc/_odocls/l/page-index.odocl

No html is generated for Bar

  $ find _build/default -name '*.html' | sort -n
  _build/default/_doc/_html/index.html
  _build/default/_doc/_html/l/L/index.html
  _build/default/_doc/_html/l/index.html
