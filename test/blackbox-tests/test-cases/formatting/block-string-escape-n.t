Escaped \n in a block string should produce the same string value as a literal
newline between block string lines.

  $ cat > dune-project << EOF
  > (lang dune 3.17)
  > EOF

With \n escape in a block string:

  $ cat > dune << 'EOF'
  > (rule
  >  (alias escaped)
  >  (action
  >   (progn
  >    (echo "\| foo\nbar
  > )
  >    (echo escaped))))
  > EOF

  $ dune build @escaped 2>&1
  foo
  bar
  escaped

With actual newline (two block string lines):

  $ cat > dune << 'EOF'
  > (rule
  >  (alias literal)
  >  (action
  >   (progn
  >    (echo "\| foo
  >          "\| bar
  > )
  >    (echo literal))))
  > EOF

  $ dune build @literal 2>&1
  foo
  bar
  literal
