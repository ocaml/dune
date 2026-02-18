Testing interaction of dune describe and tests stanza:

  $ cat > dune-project <<EOF
  > (lang dune 3.20)
  > EOF

  $ cat > dune <<EOF
  > (tests
  >  (names foo))
  > EOF

  $ cat > foo.ml

  $ dune describe
  ((root
    $TESTCASE_ROOT)
   (build_context _build/default))

Compared to an executables stanza:

  $ cat > dune <<EOF
  > (executables
  >  (names foo))
  > EOF

  $ dune describe
  ((root
    $TESTCASE_ROOT)
   (build_context _build/default)
   (executables
    ((names (foo))
     (requires ())
     (modules
      (((name Foo)
        (impl (_build/default/foo.ml))
        (intf ())
        (cmt (_build/default/.foo.eobjs/byte/dune__exe__Foo.cmt))
        (cmti ()))))
     (include_dirs (_build/default/.foo.eobjs/byte)))))
