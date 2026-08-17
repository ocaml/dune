Dune currently accepts META file templates that findlib rejects. Each case
builds and installs before findlib reports the incompatible construct.

  $ setup_project () {
  >   name="$1"
  >   mkdir "$name"
  >   cat >"$name/dune-project" <<EOF
  > (lang dune 2.7)
  > (package (name $name))
  > EOF
  >   cat >"$name/dune" <<EOF
  > (library (public_name $name))
  > EOF
  >   echo 'let value = ()' >"$name/$name.ml"
  > }

Unknown escapes are not part of findlib's quoted-string syntax.

  $ setup_project unknown_escape
  $ cat >unknown_escape/META.unknown_escape.template <<'EOF'
  > description = "unknown\qescape"
  > # DUNE_GEN
  > EOF
  $ (cd unknown_escape && dune build @install)
  $ ocamlfind lint unknown_escape/_build/default/META.unknown_escape
  ocamlfind: Cannot parse 'unknown_escape/_build/default/META.unknown_escape': Error in 'name = value' clause at line 1 position 0
  [2]

Predicate lists must be non-empty and may not end with a comma.

  $ setup_project empty_predicates
  $ cat >empty_predicates/META.empty_predicates.template <<'EOF'
  > description() = "empty"
  > # DUNE_GEN
  > EOF
  $ (cd empty_predicates && dune build @install)
  $ ocamlfind lint empty_predicates/_build/default/META.empty_predicates
  ocamlfind: Cannot parse 'empty_predicates/_build/default/META.empty_predicates': Error in 'name = value' clause at line 1 position 0
  [2]

  $ setup_project trailing_comma
  $ cat >trailing_comma/META.trailing_comma.template <<'EOF'
  > description(foo,) = "trailing"
  > # DUNE_GEN
  > EOF
  $ (cd trailing_comma && dune build @install)
  $ ocamlfind lint trailing_comma/_build/default/META.trailing_comma
  ocamlfind: Cannot parse 'trailing_comma/_build/default/META.trailing_comma': Error in 'name = value' clause at line 1 position 0
  [2]

Findlib rejects duplicate base definitions after normalizing their predicates.

  $ setup_project duplicate_definition
  $ cat >duplicate_definition/META.duplicate_definition.template <<'EOF'
  > description(foo,bar) = "first"
  > description(bar,foo,foo) = "second"
  > # DUNE_GEN
  > EOF
  $ (cd duplicate_definition && dune build @install)
  $ ocamlfind lint duplicate_definition/_build/default/META.duplicate_definition
  ocamlfind: Cannot parse 'duplicate_definition/_build/default/META.duplicate_definition': Double definition of 'description(bar,foo)'
  [2]

Rendering can introduce a duplicate base definition into a source template that
is valid on its own.

  $ setup_project rendered_duplicate
  $ cat >rendered_duplicate/dune <<'EOF'
  > (library
  >  (public_name rendered_duplicate)
  >  (synopsis "generated"))
  > EOF
  $ cat >rendered_duplicate/META.rendered_duplicate.template <<'EOF'
  > description = "template"
  > # DUNE_GEN
  > EOF
  $ (cd rendered_duplicate && dune build @install)
  $ ocamlfind lint rendered_duplicate/_build/default/META.rendered_duplicate
  ocamlfind: Cannot parse 'rendered_duplicate/_build/default/META.rendered_duplicate': Double definition of 'description'
  [2]

Sibling subpackages must have distinct names.

  $ setup_project duplicate_package
  $ cat >duplicate_package/META.duplicate_package.template <<'EOF'
  > package "dup" (
  >  description = "first"
  > )
  > package "dup" (
  >  description = "second"
  > )
  > # DUNE_GEN
  > EOF
  $ (cd duplicate_package && dune build @install)
  $ ocamlfind lint duplicate_package/_build/default/META.duplicate_package
  ocamlfind: Cannot parse 'duplicate_package/_build/default/META.duplicate_package': Double definition for subpackage dup
  [2]

The compatible forms remain accepted by both parsers.

  $ setup_project compatible
  $ cat >compatible/META.compatible.template <<'EOF'
  > custom(foo) = "first"
  > custom(bar) = "second"
  > custom(foo,foo,bar) += "third"
  > custom(foo,foo,bar) += "fourth"
  > quoted = "quote: \"; slash: \\"
  > package "left" (package "same" (custom = "left"))
  > package "right" (package "same" (custom = "right"))
  > # DUNE_GEN
  > EOF
  $ (cd compatible && dune build @install)
  $ OCAMLPATH=$PWD/compatible/_build/install/default/lib OCAMLFIND_LDCONF=ignore \
  > ocamlfind query compatible
  $TESTCASE_ROOT/compatible/_build/install/default/lib/compatible
