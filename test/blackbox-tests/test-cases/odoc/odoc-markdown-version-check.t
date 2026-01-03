Testing that odoc markdown generation is only enabled with odoc >= 3.1.0

  $ cat > dune-project <<EOF
  > (lang dune 3.0)
  > (package (name foo))
  > EOF

  $ cat > dune <<EOF
  > (library
  >  (public_name foo)
  >  (name foo))
  > EOF

  $ cat > foo.ml <<EOF
  > (** This is a test module *)
  > let x = 42
  > EOF

  $ cat > foo.mli <<EOF
  > (** This is the interface for the test module *)
  > val x : int
  > (** The answer to everything *)
  > EOF

Test with odoc 3.1.0 (supports markdown):
  $ stub_odoc_310

  $ dune build @doc-markdown
  $ ls _build/default/_doc/_markdown/foo/index.md
  _build/default/_doc/_markdown/foo/index.md

  $ dune clean

Test with odoc 2.0.0 (markdown not supported):
  $ stub_odoc_200

Without verbose, no warning is shown:
  $ dune build @doc-markdown 2>&1 | grep -i "warning"
  [1]

With --display verbose, the warning is shown:
  $ dune build @doc-markdown --display verbose 2>&1 | grep -i "warning\|version 3.1.0"
  Warning: odoc version 2.0.0 is installed, but markdown documentation requires
  version 3.1.0 or higher.
  $ test -d _build/default/_doc/_markdown
  [1]

  $ dune clean

Test with invalid version string:
  $ stub_odoc_bad_version

Without verbose, no warning is shown:
  $ dune build @doc-markdown 2>&1 | grep -i "warning"
  [1]

With --display verbose, the warning is shown:
  $ dune build @doc-markdown --display verbose 2>&1 | grep -i "warning\|version 3.1.0"
  Warning: Could not determine odoc version. Markdown documentation requires
  odoc version 3.1.0 or higher.
  $ test -d _build/default/_doc/_markdown
  [1]
