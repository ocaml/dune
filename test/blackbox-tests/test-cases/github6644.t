staged_pps should work even if sandboxing is enabled

Regression test for #6644

  $ cat >dune-project <<EOF
  > (lang dune 3.6)
  > (package
  >  (name foo))
  > EOF

  $ cat >dune <<EOF
  > (library
  >  (name driver1)
  >  (public_name foo.driver)
  >  (modules ())
  >  (ppx.driver (main "(fun () -> assert false)")))
  > (library
  >  (name dummy_rewriter)
  >  (modules ())
  >  (libraries driver1)
  >  (kind ppx_rewriter))
  > (library
  >  (name foo)
  >  (modules foo)
  >  (preprocess (staged_pps dummy_rewriter)))
  > EOF
  $ touch foo.ml

  $ dune build foo.cma 2>&1 | grep Assert_failure | sed 's/\(.* Assert_failure\).*/\1/g'
  Fatal error: exception Assert_failure
