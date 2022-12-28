Too many drivers

  $ dune build foo2.cma
  File "dune", line 6, characters 13-28:
  6 |  (preprocess (pps ppx1 ppx2)))
                   ^^^^^^^^^^^^^^^
  Error: Too many incompatible ppx drivers were found: foo.driver1 and
  foo.driver2.
  [1]

Not compatible with Dune

  $ dune build foo3.cma
  File "dune", line 13, characters 13-28:
  13 |  (preprocess (pps ppx_other)))
                    ^^^^^^^^^^^^^^^
  Error: No ppx driver were found. It seems that ppx_other is not compatible
  with Dune. Examples of ppx rewriters that are compatible with Dune are ones
  using ocaml-migrate-parsetree, ppxlib or ppx_driver.
  [1]

Incompatible Cookies

  $ dune build foo4.cma
  File "dune", line 20, characters 13-28:
  20 |  (preprocess (pps ppx3 ppx4)))
                    ^^^^^^^^^^^^^^^
  Error: foo.ppx3 and foo.ppx4 have inconsistent requests for cookie "germany";
  foo.ppx3 requests "spritzgeback" and foo.ppx4 requests "lebkuchen"
  [1]

Same, but with error pointing to .ppx

  $ dune build .ppx/foo.ppx1+foo.ppx2/ppx.exe
  Error: invalid ppx key for _build/default/.ppx/foo.ppx1+foo.ppx2/ppx.exe
  [1]

  $ dune build .ppx/foo.ppx-other/ppx.exe
  Error: invalid ppx key for _build/default/.ppx/foo.ppx-other/ppx.exe
  [1]

Test the argument syntax

  $ dune build test_ppx_args.cma
  .ppx/454728df5270ab91f8a5af6b5e860eb0/ppx.exe
  -arg1
  -arg2
  -arg3=Oreo
  -foo
  bar
  Snickerdoodle
  --cookie
  france="Petit Beurre"
  --cookie
  italy="Biscotti"
  --cookie
  library-name="test_ppx_args"
  -o
  test_ppx_args.pp.ml
  --impl
  test_ppx_args.ml
  --as-ppx
  File "dune", line 94, characters 3-138:
  94 |    (pps -arg1 driver_print_args ppx_with_cookies_print_args -arg2 -arg3=%{env:AMERICA=undefined} --
  95 |     -foo bar %{env:ENGLAND=undefined})))
  Error: Rule failed to generate the following targets:
  - test_ppx_args.pp.ml
  [1]

Test that going through the -ppx option of the compiler works

  $ dune build test_ppx_staged.cma
  tool name: ocamlc
  args:--as-ppx -arg1 -arg2 -arg3=Oreo -foo bar Snickerdoodle --cookie france="Petit Beurre" --cookie italy="Biscotti" --cookie library-name="test_ppx_staged"
