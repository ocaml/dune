coq.extraction should work in the same directory as a coq.theory.

  $ cat > dune-project << EOF
  > (lang dune 3.9)
  > (using coq 0.8)
  > EOF

  $ cat > dune << EOF
  > (coq.theory
  >  (modules :standard \ extract)
  >  (name Foo))
  > 
  > (coq.extraction
  >  (prelude extract)
  >  (extracted_modules Datatypes extract)
  >  (theories Foo))
  > 
  > (executable
  >  (name foo))
  > EOF

  $ dune build
  Warning:
  $TESTCASE_ROOT/_build/default
  was previously bound to Foo; it is remapped to DuneExtraction
  [overriding-logical-loadpath,loadpath]
