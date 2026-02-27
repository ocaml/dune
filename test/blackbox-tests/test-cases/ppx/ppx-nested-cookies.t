Reproduction for https://github.com/ocaml/dune/issues/3426
Cookies defined on a ppx_rewriter are lost when that rewriter is wrapped
inside another ppx_rewriter.

Set up a PPX driver that accepts --cookie and produces a valid output file,
an inner ppx with cookies, and a wrapper ppx that depends on the inner one:

  $ export DUNE_TRACE="process"
  $ cat > dune-project <<EOF
  > (lang dune 3.0)
  > EOF
  $ cat > dune <<EOF
  > (library
  >  (name driver)
  >  (modules driver)
  >  (ppx.driver (main Driver.main)))
  > (library
  >  (name inner_ppx)
  >  (kind (ppx_rewriter (cookies (my_cookie "the_value"))))
  >  (modules ())
  >  (libraries driver))
  > (library
  >  (name wrapper_ppx)
  >  (kind ppx_rewriter)
  >  (modules ())
  >  (libraries inner_ppx))
  > (rule (with-stdout-to direct.ml (echo "")))
  > (rule (with-stdout-to wrapped.ml (echo "")))
  > (library
  >  (name test_direct)
  >  (modules direct)
  >  (preprocess (pps inner_ppx)))
  > (library
  >  (name test_wrapped)
  >  (modules wrapped)
  >  (preprocess (pps wrapper_ppx)))
  > EOF
  $ cat > driver.ml <<EOF
  > let main () =
  >   let out = ref "" in
  >   let args =
  >     [ ("-o", Arg.Set_string out, "")
  >     ; ("--impl", Arg.Set_string (ref ""), "")
  >     ; ("--as-ppx", Arg.Set (ref false), "")
  >     ; ("--cookie", Arg.String (fun _ -> ()), "")
  >     ]
  >   in
  >   Arg.parse (Arg.align args) (fun _ -> ()) "";
  >   let oc = open_out !out in
  >   close_out oc
  > EOF

Using the inner ppx directly, the cookie is passed:

  $ dune build test_direct.cma
  $ dune trace cat | jq -c 'include "dune";
  >   processes
  > | select(.args.prog | contains("ppx"))
  > | .args.process_args
  > | map(select(startswith("my_cookie")))
  > | select(length > 0)
  > '
  ["my_cookie=\"the_value\""]

Using the wrapper ppx, the cookie from inner_ppx should also be passed
(but is currently lost due to the bug):

  $ dune build test_wrapped.cma
  $ dune trace cat | jq -c 'include "dune";
  >   processes
  > | select(.args.prog | contains("ppx"))
  > | .args.process_args
  > | map(select(startswith("my_cookie")))
  > | select(length > 0)
  > '
