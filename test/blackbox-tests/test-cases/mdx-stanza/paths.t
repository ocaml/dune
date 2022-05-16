Absolute paths cause an error.

  $ set_version () {
  >   sed -i.bak "s/using mdx .../using mdx $1/" dune-project
  > }

  $ cat > dune-project << EOF
  > (lang dune 3.2)
  > (using mdx ---)
  > EOF
  $ set_version 0.2
  $ cat > dune << EOF
  > (mdx)
  > EOF

  $ cat > README.md << 'EOF'
  > ```ocaml file=/etc/passwd
  > ```
  > EOF

  $ dune runtest
  File "dune", line 1, characters 0-5:
  1 | (mdx)
      ^^^^^
  Error: Paths referenced in mdx files must be relative. This stanza refers to
  the following absolute path:
  Source path: README.md
  Included path: /etc/passwd
  [1]

Relative paths that go over the root cause an error.

  $ cat > README.md << 'EOF'
  > ```ocaml file=../x
  > ```
  > EOF

  $ dune runtest
  File "dune", line 1, characters 0-5:
  1 | (mdx)
      ^^^^^
  Error: Paths referenced in mdx files must stay within the workspace. This
  stanza refers to the following path which escapes:
  Source path: README.md
  Included path: ../x
  [1]

Relative paths within the workspace do not work.

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
  File "a/dune", line 1, characters 0-5:
  1 | (mdx)
      ^^^^^
  Error: Paths referenced in mdx files cannot escape the directory. This stanza
  refers to the following path which escapes:
  Source path: a/README.md
  Included path: ../b/src.ml
  [1]

But this works with stanza 0.3:

  $ set_version 0.3
  $ dune runtest
  File "a/README.md", line 1, characters 0-0:
  Error: Files _build/default/a/README.md and
  _build/default/a/.mdx/README.md.corrected differ.
  [1]
  $ set_version 0.2

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
