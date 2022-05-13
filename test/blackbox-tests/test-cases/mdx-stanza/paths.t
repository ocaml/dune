Absolute paths cause an error.

  $ cat > dune-project << EOF
  > (lang dune 3.0)
  > (using mdx 0.2)
  > EOF
  $ cat > dune << EOF
  > (mdx)
  > EOF

  $ cat > README.md << 'EOF'
  > ```ocaml file=/etc/passwd
  > ```
  > EOF

  $ dune runtest 2>&1 | awk '/Internal error/,/Raised at/'
  Internal error, please report upstream including the contents of _build/log.
  Description:
    ("Local.relative: received absolute path",
    { t = "."; path = "/etc/passwd" })
  Raised at Stdune__Code_error.raise in file "otherlibs/stdune/code_error.ml",

Relative paths that go over the root cause an error.

  $ cat > README.md << 'EOF'
  > ```ocaml file=../x
  > ```
  > EOF

  $ dune runtest
  Error: path outside the workspace: ../x from .
  -> required by _build/default/.mdx/README.md.corrected
  -> required by alias runtest in dune:1
  [1]

Relative paths within the workspace should work.

  $ mkdir a b
  $ mv dune a/
  $ rm README.md
  $ cat > b/src.ml << EOF
  > let x = 1
  > EOF

  $ cat > a/README.md << 'EOF'
  > ```ocaml file=../b/src.ml
  > ```
  > EOF

  $ dune runtest
  Error: path outside the workspace: ../b/src.ml from .
  -> required by _build/default/a/.mdx/README.md.corrected
  -> required by alias a/runtest in a/dune:1
  [1]

Files in the same directory work.

  $ mv b/src.ml a/
  $ cat > a/README.md << 'EOF'
  > ```ocaml file=src.ml
  > ```
  > EOF

  $ dune runtest
  File "a/README.md", line 1, characters 0-0:
  Error: Files _build/default/a/README.md and
  _build/default/a/.mdx/README.md.corrected differ.
  [1]

From a subdirectory too:

  $ mkdir a/sub/
  $ mv a/src.ml a/sub/
  $ cat > a/README.md << 'EOF'
  > ```ocaml file=sub/src.ml
  > ```
  > EOF

  $ dune runtest
  File "a/README.md", line 1, characters 0-0:
  Error: Files _build/default/a/README.md and
  _build/default/a/.mdx/README.md.corrected differ.
  [1]
