Module artifact variables should use the Melange module set when a library
supports both OCaml and Melange, but a module is only selected for Melange.

  $ make_melange_project 3.24 1.0

  $ mkdir lib
  $ cat > lib/dune <<'EOF'
  > (library
  >  (name foo)
  >  (modes byte melange)
  >  (modules common ocaml_only)
  >  (melange.modules common melange_only))
  > EOF

  $ cat > lib/common.ml <<'EOF'
  > let x = "common"
  > EOF

  $ cat > lib/ocaml_only.ml <<'EOF'
  > let x = "ocaml"
  > EOF

  $ cat > lib/melange_only.ml <<'EOF'
  > let x = "melange"
  > EOF

When a module is selected in both modes, artifact variables prefer the OCaml
artifact, like Merlin does.

  $ dune build '%{cmi:lib/common}'
  $ dune trace cat | jq 'select(.name == "targets") | .args'
  {
    "targets": [
      "_build/default/lib/.foo.objs/byte/foo__Common.cmi"
    ]
  }

The cmj variable always selects the Melange artifact.

  $ dune build '%{cmj:lib/common}'
  $ dune trace cat | jq 'select(.name == "targets") | .args'
  {
    "targets": [
      "_build/default/lib/.foo.objs/melange/foo__Common.cmj"
    ]
  }

The Melange-only module artifacts exist in the Melange object directory.

  $ dune build lib/.foo.objs/melange/foo__Melange_only.{cmi,cmt}

Artifact variables fall back to the Melange module set when the module is not
selected for OCaml.

  $ dune build '%{cmi:lib/melange_only}'
  $ dune build '%{cmt:lib/melange_only}'

OCaml-specific artifact variables do not fall back to the Melange module set.

  $ dune build '%{cmo:lib/melange_only}'
  File "command line", line 1, characters 0-23:
  Error: Module Melange_only does not exist.
  [1]
  $ dune build '%{cmx:lib/melange_only}'
  File "command line", line 1, characters 0-23:
  Error: Module Melange_only does not exist.
  [1]
