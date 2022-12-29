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

  $ dune build foo.cma
  File ".foo.objs/byte/_unknown_", line 1, characters 0-0:
  Error: This rule forbids all sandboxing modes (but it also requires
  sandboxing)
  [1]
