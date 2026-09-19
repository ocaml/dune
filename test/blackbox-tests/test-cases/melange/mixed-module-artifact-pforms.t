Module artifact variables should use the Melange module set when a library
supports both OCaml and Melange, but a module is only selected for Melange.

  $ make_melange_project 3.24 1.0

  $ mkdir lib
  $ cat > lib/dune <<'EOF'
  > (library
  >  (name foo)
  >  (modes byte melange)
  >  (modules common ocaml_only mode_specific)
  >  (melange.modules common melange_only mode_specific))
  > EOF

  $ cat > lib/common.ml <<'EOF'
  > let x = "common"
  > EOF

  $ cat > lib/common.mli <<'EOF'
  > val x : string
  > EOF

  $ cat > lib/ocaml_only.ml <<'EOF'
  > let x = "ocaml"
  > EOF

  $ cat > lib/melange_only.ml <<'EOF'
  > let x = "melange"
  > EOF

  $ cat > lib/mode_specific.ml <<'EOF'
  > let x = 1
  > EOF
  $ cat > lib/mode_specific.melange.mli <<'EOF'
  > val x : int
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

  $ dune build '%{cmt:lib/common}' '%{cmti:lib/common}'
  $ dune trace cat | jq 'select(.name == "targets") | .args'
  {
    "targets": [
      "_build/default/lib/.foo.objs/byte/foo__Common.cmt",
      "_build/default/lib/.foo.objs/byte/foo__Common.cmti"
    ]
  }

The melange.cmi variable explicitly selects the Melange artifact, including
when the module is selected in both modes.

  $ dune build '%{melange.cmi:lib/common}'
  $ dune trace cat | jq 'select(.name == "targets") | .args'
  {
    "targets": [
      "_build/default/lib/.foo.objs/melange/foo__Common.cmi"
    ]
  }

It also selects modules that are only compiled with Melange.

  $ dune build '%{melange.cmi:lib/melange_only}'

The explicit annotation variables also select Melange artifacts. The
melange.cmj variable is an alias for cmj.

  $ dune build '%{melange.cmt:lib/common}'
  $ dune trace cat | jq 'select(.name == "targets") | .args'
  {
    "targets": [
      "_build/default/lib/.foo.objs/melange/foo__Common.cmt"
    ]
  }
  $ dune build '%{melange.cmti:lib/common}'
  $ dune trace cat | jq 'select(.name == "targets") | .args'
  {
    "targets": [
      "_build/default/lib/.foo.objs/melange/foo__Common.cmti"
    ]
  }
  $ dune build '%{melange.cmj:lib/common}'
  $ dune trace cat | jq 'select(.name == "targets") | .args'
  {
    "targets": [
      "_build/default/lib/.foo.objs/melange/foo__Common.cmj"
    ]
  }

The explicit variables do not fall back to the OCaml module set.

  $ dune build '%{melange.cmi:lib/ocaml_only}'
  File "command line", line 1, characters 0-29:
  Error: Module Ocaml_only does not exist.
  [1]
  $ dune build '%{melange.cmt:lib/ocaml_only}'
  File "command line", line 1, characters 0-29:
  Error: Module Ocaml_only does not exist.
  [1]
  $ dune build '%{melange.cmti:lib/ocaml_only}'
  File "command line", line 1, characters 0-30:
  Error: Module Ocaml_only does not exist.
  [1]
  $ dune build '%{melange.cmj:lib/ocaml_only}'
  File "command line", line 1, characters 0-29:
  Error: Module Ocaml_only does not exist.
  [1]

Without an explicit interface, melange.cmti falls back to the implementation's
annotation file, just like cmti.

  $ dune build '%{melange.cmt:lib/melange_only}' '%{melange.cmti:lib/melange_only}'
  $ dune trace cat | jq 'select(.name == "targets") | .args'
  {
    "targets": [
      "_build/default/lib/.foo.objs/melange/foo__Melange_only.cmt",
      "_build/default/lib/.foo.objs/melange/foo__Melange_only.cmt"
    ]
  }
  $ dune build '%{melange.cmj:lib/melange_only}'

The choice of annotation file depends on the sources selected for each mode.
This module has a Melange-specific interface, but no OCaml interface.

  $ dune build '%{cmti:lib/mode_specific}' '%{melange.cmti:lib/mode_specific}'
  $ dune trace cat | jq 'select(.name == "targets") | .args'
  {
    "targets": [
      "_build/default/lib/.foo.objs/byte/foo__Mode_specific.cmt",
      "_build/default/lib/.foo.objs/melange/foo__Mode_specific.cmti"
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

The cmj variable does not fall back to the OCaml module set.

  $ dune build '%{cmj:lib/ocaml_only}'
  File "command line", line 1, characters 0-21:
  Error: Module Ocaml_only does not exist.
  [1]

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

The explicit variables are available in dune files since version 3.26 of the
Dune language.

  $ mkdir version-gate
  $ cat > version-gate/dune-project <<'EOF'
  > (lang dune 3.25)
  > (using melange 1.0)
  > EOF
  $ test_version () {
  >   cat > version-gate/dune <<EOF
  > (library
  >  (name foo)
  >  (modes melange))
  > (alias
  >  (name artifact)
  >  (deps %{melange.$1:foo}))
  > EOF
  >   dune build --root=version-gate @artifact
  > }
  $ touch version-gate/foo.ml version-gate/foo.mli
  $ test_version cmi
  Entering directory 'version-gate'
  File "dune", line 6, characters 7-25:
  6 |  (deps %{melange.cmi:foo}))
             ^^^^^^^^^^^^^^^^^^
  Error: %{melange.cmi:..} is only available since version 3.26 of the dune
  language. Please update your dune-project file to have (lang dune 3.26).
  Leaving directory 'version-gate'
  [1]
  $ test_version cmt
  Entering directory 'version-gate'
  File "dune", line 6, characters 7-25:
  6 |  (deps %{melange.cmt:foo}))
             ^^^^^^^^^^^^^^^^^^
  Error: %{melange.cmt:..} is only available since version 3.26 of the dune
  language. Please update your dune-project file to have (lang dune 3.26).
  Leaving directory 'version-gate'
  [1]
  $ test_version cmti
  Entering directory 'version-gate'
  File "dune", line 6, characters 7-26:
  6 |  (deps %{melange.cmti:foo}))
             ^^^^^^^^^^^^^^^^^^^
  Error: %{melange.cmti:..} is only available since version 3.26 of the dune
  language. Please update your dune-project file to have (lang dune 3.26).
  Leaving directory 'version-gate'
  [1]
  $ test_version cmj
  Entering directory 'version-gate'
  File "dune", line 6, characters 7-25:
  6 |  (deps %{melange.cmj:foo}))
             ^^^^^^^^^^^^^^^^^^
  Error: %{melange.cmj:..} is only available since version 3.26 of the dune
  language. Please update your dune-project file to have (lang dune 3.26).
  Leaving directory 'version-gate'
  [1]

  $ cat > version-gate/dune-project <<'EOF'
  > (lang dune 3.26)
  > (using melange 1.0)
  > EOF
  $ test_version cmi
  $ test_version cmt
  $ test_version cmti
  $ test_version cmj

For an interface-only module, melange.cmt and melange.cmj expand to an empty
string, whereas melange.cmti selects the interface's annotation file.

  $ mkdir version-gate/interface-only
  $ cat > version-gate/interface-only/dune <<'EOF'
  > (library
  >  (name intf_only)
  >  (modes melange)
  >  (modules_without_implementation intf_only))
  > (rule
  >  (alias artifact)
  >  (action
  >   (progn
  >    (echo "<%{melange.cmt:intf_only}> <%{melange.cmj:intf_only}>\n")
  >    (echo %{melange.cmti:intf_only}))))
  > EOF
  $ touch version-gate/interface-only/intf_only.mli
  $ dune build --root=version-gate @interface-only/artifact
  <> <>
  .intf_only.objs/melange/intf_only.cmti
