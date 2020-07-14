Run inline tests using node js

  $ cat >dune-project <<EOF
  > (lang dune 2.6)
  > EOF

  $ dune runtest
           run alias byte/runtest
  inline tests (Byte)
  inline tests (Byte)
           run alias native/runtest
  inline tests (Native)
  inline tests (Native)
          node alias js/runtest
  inline tests (JS)
  inline tests (JS)
