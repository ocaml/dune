Escaped \n in a block string should produce the same string value as a literal
newline between block string lines.

  $ cat > dune-project << EOF
  > (lang dune 3.17)
  > EOF

With \n escape in a block string:

  $ cat > dune << 'EOF'
  > (rule
  >  (alias default)
  >  (action
  >   (echo "\| foo\nbar
  > )))
  > EOF

  $ dune build 2>&1
  foo
  bar

With actual newline (two block string lines):

  $ cat > dune << 'EOF'
  > (rule
  >  (alias default)
  >  (action
  >   (echo "\| foo
  >         "\| bar
  > )))
  > EOF

  $ dune build 2>&1
