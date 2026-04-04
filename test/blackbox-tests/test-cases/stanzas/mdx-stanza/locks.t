Version 0.2 of the mdx stanza does not support (locks):

  $ cat > dune-project << EOF
  > (lang dune 3.2)
  > (using mdx 0.2)
  > EOF

  $ cat > dune << EOF
  > (mdx
  >  (locks l))
  > EOF

  $ dune build
  File "dune", line 2, characters 1-10:
  2 |  (locks l))
       ^^^^^^^^^
  Error: 'locks' is only available since version 0.3 of mdx extension to verify
  code blocks in .md files. Please update your dune-project file to have (using
  mdx 0.3).
  [1]

In version 0.3, it is accepted:

  $ cat > dune-project << EOF
  > (lang dune 3.2)
  > (using mdx 0.3)
  > EOF

  $ dune build
