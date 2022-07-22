No ppx driver found

  $ mkdir -p no-driver
  $ echo '(lang dune 2.8)' > no-driver/dune-project
  $ cat >no-driver/dune <<EOF
  > (library
  >  (name foo1)
  >  (public_name foo.1)
  >  (modules foo1)
  >  (preprocess (pps)))
  > EOF
  $ dune build --root no-driver
  Entering directory 'no-driver'
  File "dune", line 5, characters 13-18:
  5 |  (preprocess (pps)))
                   ^^^^^
  Error: You must specify at least one ppx rewriter.
  [1]

Too many drivers

  $ dune build --root driver-tests foo2.cma
  Entering directory 'driver-tests'
  File "dune", line 6, characters 13-28:
  6 |  (preprocess (pps ppx1 ppx2)))
                   ^^^^^^^^^^^^^^^
  Error: Too many incompatible ppx drivers were found: foo.driver1 and
  foo.driver2.
  [1]

Not compatible with Dune

  $ dune build --root driver-tests foo3.cma
  Entering directory 'driver-tests'
  File "dune", line 13, characters 13-28:
  13 |  (preprocess (pps ppx_other)))
                    ^^^^^^^^^^^^^^^
  Error: No ppx driver were found. It seems that ppx_other is not compatible
  with Dune. Examples of ppx rewriters that are compatible with Dune are ones
  using ocaml-migrate-parsetree, ppxlib or ppx_driver.
  [1]

Incompatible Cookies

  $ dune build --root driver-tests foo4.cma
  Entering directory 'driver-tests'
  File "dune", line 20, characters 13-28:
  20 |  (preprocess (pps ppx3 ppx4)))
                    ^^^^^^^^^^^^^^^
  Error: foo.ppx3 and foo.ppx4 have inconsistent requests for cookie "germany";
  foo.ppx3 requests "spritzgeback" and foo.ppx4 requests "lebkuchen"
  [1]

Same, but with error pointing to .ppx

  $ dune build --root driver-tests .ppx/foo.ppx1+foo.ppx2/ppx.exe
  Entering directory 'driver-tests'
  Error: invalid ppx key for default/.ppx/foo.ppx1+foo.ppx2/ppx.exe
  [1]

  $ dune build --root driver-tests .ppx/foo.ppx-other/ppx.exe
  Entering directory 'driver-tests'
  Error: invalid ppx key for default/.ppx/foo.ppx-other/ppx.exe
  [1]

Test the argument syntax

  $ dune build --root driver-tests test_ppx_args.cma
  Entering directory 'driver-tests'
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

Test the argument syntax with list expansion allowed (dune > 3.2)

  $ dune build --root driver-tests-list-args
  Entering directory 'driver-tests-list-args'
  .ppx/454728df5270ab91f8a5af6b5e860eb0/ppx.exe
  -arg1
  -arg2
  -arg3=Oreo
  -foo
  bar
  Snickerdoodle
  Args
  From
  A
  File
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
  File "dune", line 23, characters 3-161:
  23 |    (pps -arg1 driver_print_args ppx_with_cookies_print_args -arg2 -arg3=%{env:AMERICA=undefined} --
  24 |     -foo bar %{env:ENGLAND=undefined} %{read-lines:ppx-args})))
  Error: Rule failed to generate the following targets:
  - test_ppx_args.pp.ml
  [1]

Test that going through the -ppx option of the compiler works

  $ dune build --root driver-tests test_ppx_staged.cma
  Entering directory 'driver-tests'
  tool name: ocamlc
  args:--as-ppx -arg1 -arg2 -arg3=Oreo -foo bar Snickerdoodle --cookie france="Petit Beurre" --cookie italy="Biscotti" --cookie library-name="test_ppx_staged"

Test using installed drivers

  $ dune build --root driver @install
  Entering directory 'driver'
  $ OCAMLPATH=driver/_build/install/default/lib dune build --root use-external-driver driveruser.cma
  Entering directory 'use-external-driver'
  .ppx/35d69311d5da258d073875db2b34f33b/ppx.exe
  -arg1
  -arg2
  -foo
  bar
  --cookie
  library-name="driveruser"
  -o
  driveruser.pp.ml
  --impl
  driveruser.ml
  --as-ppx
  File "dune", line 6, characters 13-53:
  6 |  (preprocess (pps -arg1 testdriver -arg2 -- -foo bar)))
                   ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
  Error: Rule failed to generate the following targets:
  - driveruser.pp.ml
  [1]

  $ OCAMLPATH=driver/_build/install/default/lib dune build --root replaces driveruser.cma
  Entering directory 'replaces'
  replacesdriver
  .ppx/886937db0da323b743b4366c6d3a795f/ppx.exe
  -arg1
  -arg2
  -foo
  bar
  --cookie
  library-name="driveruser"
  -o
  driveruser.pp.ml
  --impl
  driveruser.ml
  --as-ppx
  File "dune", line 13, characters 13-57:
  13 |  (preprocess (pps -arg1 replacesdriver -arg2 -- -foo bar)))
                    ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
  Error: Rule failed to generate the following targets:
  - driveruser.pp.ml
  [1]

  $ OCAMLPATH=driver/_build/install/default/lib dune build --root driver-replaces @install
  Entering directory 'driver-replaces'
  $ OCAMLPATH=driver/_build/install/default/lib:driver-replaces/_build/install/default/lib dune build --root replaces-external driveruser.cma
  Entering directory 'replaces-external'
  replacesdriver
  .ppx/886937db0da323b743b4366c6d3a795f/ppx.exe
  -arg1
  -arg2
  -foo
  bar
  --cookie
  library-name="driveruser"
  -o
  driveruser.pp.ml
  --impl
  driveruser.ml
  --as-ppx
  File "dune", line 5, characters 13-57:
  5 |  (preprocess (pps -arg1 replacesdriver -arg2 -- -foo bar)))
                   ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
  Error: Rule failed to generate the following targets:
  - driveruser.pp.ml
  [1]
