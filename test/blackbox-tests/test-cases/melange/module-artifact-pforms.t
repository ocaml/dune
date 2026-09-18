Module artifact variables should resolve artifacts produced by Melange-only
libraries.

  $ make_melange_project 3.21 1.0

  $ cat > dune <<'EOF'
  > (library
  >  (name foo)
  >  (modes melange))
  > EOF

  $ cat > foo.mli <<'EOF'
  > val x : int
  > EOF

  $ cat > foo.ml <<'EOF'
  > let x = 42
  > EOF

The Melange compiler produces all four module artifacts.

  $ dune build .foo.objs/melange/foo.{cmi,cmj,cmt,cmti}
  $ ls _build/default/.foo.objs/melange/foo.* | sort
  _build/default/.foo.objs/melange/foo.cmi
  _build/default/.foo.objs/melange/foo.cmj
  _build/default/.foo.objs/melange/foo.cmt
  _build/default/.foo.objs/melange/foo.cmti

Module artifact variables resolve artifacts from the Melange object directory
for a Melange-only library.

  $ dune build '%{cmi:foo}'
  $ dune build '%{cmt:foo}'
  $ dune build '%{cmti:foo}'

The cmj variable resolves Melange's compiled module artifact.

  $ dune build '%{cmj:foo}'

The cmj variable is available in dune files since Dune 3.26. CLI arguments use
the latest language version, which is why the command above succeeds in this
Dune 3.21 project.

  $ mkdir cmj-version
  $ cat > cmj-version/dune <<'EOF'
  > (library
  >  (name foo)
  >  (modes melange))
  > (rule
  >  (alias artifact)
  >  (action (echo %{cmj:foo})))
  > EOF
  $ echo 'let x = 42' > cmj-version/foo.ml

  $ cat > cmj-version/dune-project <<'EOF'
  > (lang dune 3.25)
  > (using melange 1.0)
  > EOF
  $ dune build --root=cmj-version @artifact
  Entering directory 'cmj-version'
  File "dune", line 6, characters 15-25:
  6 |  (action (echo %{cmj:foo})))
                     ^^^^^^^^^^
  Error: %{cmj:..} is only available since version 3.26 of the dune language.
  Please update your dune-project file to have (lang dune 3.26).
  Leaving directory 'cmj-version'
  [1]

  $ cat > cmj-version/dune-project <<'EOF'
  > (lang dune 3.26)
  > (using melange 1.0)
  > EOF
  $ dune build --root=cmj-version @artifact
  .foo.objs/melange/foo.cmj

For an interface-only module, the cmj variable expands to an empty string.

  $ mkdir cmj-version/interface-only
  $ cat > cmj-version/interface-only/dune <<'EOF'
  > (library
  >  (name intf_only)
  >  (modes melange)
  >  (modules_without_implementation intf_only))
  > (rule
  >  (alias artifact)
  >  (action (echo "<%{cmj:intf_only}>")))
  > EOF
  $ touch cmj-version/interface-only/intf_only.mli
  $ dune build --root=cmj-version @interface-only/artifact
  <>
