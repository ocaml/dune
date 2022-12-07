Run inline tests using node js

  $ cat >dune-project <<EOF
  > (lang dune 2.6)
  > EOF

  $ dune runtest
  inline tests (Byte)
  inline tests (Byte)
  inline tests (JS)
  inline tests (JS)
  inline tests (Native)
  inline tests (Native)

  $ dune runtest --profile release
  inline tests (JS)
  inline tests (JS)
  inline tests (Native)
  inline tests (Native)
