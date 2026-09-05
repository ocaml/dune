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

The existing module artifact variables incorrectly resolve artifacts for a
Melange-only library to the bytecode object directory.

  $ dune build '%{cmi:foo}'
  Error: No rule found for .foo.objs/byte/foo.cmi
  -> required by %{cmi:foo} at command line:1
  [1]
  $ dune build '%{cmt:foo}'
  Error: No rule found for .foo.objs/byte/foo.cmt
  -> required by %{cmt:foo} at command line:1
  [1]
  $ dune build '%{cmti:foo}'
  Error: No rule found for .foo.objs/byte/foo.cmti
  -> required by %{cmti:foo} at command line:1
  [1]

The variable for Melange's compiled module artifact is not recognized.

  $ dune build '%{cmj:foo}'
  Usage: dune build [--help] [OPTION]… [TARGET]…
  dune: TARGET… arguments: Unknown macro %{cmj:..}
  [1]
