No ppx driver found

  $ dune build --root driver-tests foo1.cma
  Entering directory 'driver-tests'
  File "dune", line 6, characters 13-18:
  6 |  (preprocess (pps)))
                   ^^^^^
  Error: You must specify at least one ppx rewriter.
  [1]

Too many drivers

  $ dune build --root driver-tests foo2.cma
  Entering directory 'driver-tests'
  File "dune", line 13, characters 13-28:
  13 |  (preprocess (pps ppx1 ppx2)))
                    ^^^^^^^^^^^^^^^
  Error: Too many incompatible ppx drivers were found: foo.driver2 and
  foo.driver1.
  [1]

Not compatible with Dune

  $ dune build --root driver-tests foo3.cma
  Entering directory 'driver-tests'
  File "dune", line 20, characters 13-28:
  20 |  (preprocess (pps ppx_other)))
                    ^^^^^^^^^^^^^^^
  Error: No ppx driver were found. It seems that ppx_other is not compatible
  with Dune. Examples of ppx rewriters that are compatible with Dune are ones
  using ocaml-migrate-parsetree, ppxlib or ppx_driver.
  [1]

Incompatible Cookies

  $ dune build --root driver-tests foo4.cma
  Entering directory 'driver-tests'
  File "dune", line 27, characters 13-28:
  27 |  (preprocess (pps ppx3 ppx4)))
                    ^^^^^^^^^^^^^^^
  Error: foo.ppx3 and foo.ppx4 have inconsistent requests for cookie "germany";
  foo.ppx3 requests "spritzgeback" and foo.ppx4 requests "lebkuchen"
  [1]

Same, but with error pointing to .ppx

  $ dune build --root driver-tests .ppx/foo.ppx1+foo.ppx2/ppx.exe
  Entering directory 'driver-tests'
  File "_build/default/.ppx/foo.ppx1+foo.ppx2/ppx.exe", line 1, characters 0-0:
  Error: Failed to create on-demand ppx rewriter for foo.ppx1 and foo.ppx2; too
  many incompatible ppx drivers were found: foo.driver2 and foo.driver1.
  [1]

  $ dune build --root driver-tests .ppx/foo.ppx-other/ppx.exe
  Entering directory 'driver-tests'
  File "_build/default/.ppx/foo.ppx-other/ppx.exe", line 1, characters 0-0:
  Error: Failed to create on-demand ppx rewriter for foo.ppx-other; no ppx
  driver were found. It seems that foo.ppx-other is not compatible with Dune.
  Examples of ppx rewriters that are compatible with Dune are ones using
  ocaml-migrate-parsetree, ppxlib or ppx_driver.
  [1]

Test the argument syntax

  $ dune build --root driver-tests test_ppx_args.cma
  Entering directory 'driver-tests'
           ppx test_ppx_args.pp.ml
  .ppx/031272d0b003c307d6a59ce4a911dd3b/ppx.exe
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
  File "dune", line 101, characters 3-138:
  101 |    (pps -arg1 driver_print_args ppx_with_cookies_print_args -arg2 -arg3=%{env:AMERICA=undefined} --
  102 |     -foo bar %{env:ENGLAND=undefined})))
  Error: Rule failed to generate the following targets:
  - test_ppx_args.pp.ml
  [1]

Test that going throught the -ppx option of the compiler works

  $ dune build --root driver-tests test_ppx_staged.cma
  Entering directory 'driver-tests'
      ocamldep .test_ppx_staged.objs/test_ppx_staged.ml.d
  tool name: ocamldep
  args:--as-ppx -arg1 -arg2 -arg3=Oreo -foo bar Snickerdoodle --cookie france="Petit Beurre" --cookie italy="Biscotti" --cookie library-name="test_ppx_staged"
        ocamlc .test_ppx_staged.objs/byte/test_ppx_staged.{cmi,cmo,cmt}
  tool name: ocamlc
  args:--as-ppx -arg1 -arg2 -arg3=Oreo -foo bar Snickerdoodle --cookie france="Petit Beurre" --cookie italy="Biscotti" --cookie library-name="test_ppx_staged"

Test using installed drivers

  $ dune build --root driver @install
  Entering directory 'driver'
  $ OCAMLPATH=driver/_build/install/default/lib dune build --root use-external-driver driveruser.cma
  Entering directory 'use-external-driver'
           ppx driveruser.pp.ml
  .ppx/631757a4a4789e0bd29628f7a73480f7/ppx.exe
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
           ppx driveruser.pp.ml
  replacesdriver
  .ppx/97c3bec0ca445d915914eed462990a46/ppx.exe
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
           ppx driveruser.pp.ml
  replacesdriver
  .ppx/97c3bec0ca445d915914eed462990a46/ppx.exe
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

  $ dune build --display short --root jbuild-driver @all
  Entering directory 'jbuild-driver'
  File "jbuild", line 1, characters 0-0:
  Warning: jbuild files are deprecated, please convert this file to a dune file
  instead.
  Note: You can use "dune upgrade" to convert your project to dune.
        ocamlc .testdriver.objs/byte/testdriver.{cmi,cmo,cmt}
        ocamlc testdriver.cma
      ocamlopt .testdriver.objs/native/testdriver.{cmx,o}
      ocamlopt testdriver.{a,cmxa}
      ocamlopt testdriver.cmxs
      ocamlopt .ppx/631757a4a4789e0bd29628f7a73480f7/ppx.exe
           ppx test_ppx_args.pp.ml
  .ppx/631757a4a4789e0bd29628f7a73480f7/ppx.exe
  --cookie
  library-name="test_ppx_args"
  -o
  test_ppx_args.pp.ml
  --impl
  test_ppx_args.ml
  --as-ppx
  File "jbuild", line 13, characters 14-32:
  13 |   (preprocess (pps (testdriver)))))
                     ^^^^^^^^^^^^^^^^^^
  Error: Rule failed to generate the following targets:
  - test_ppx_args.pp.ml
  [1]

  $ dune build --display short --root jbuild-driver @install
  Entering directory 'jbuild-driver'
  File "jbuild", line 1, characters 0-0:
  Warning: jbuild files are deprecated, please convert this file to a dune file
  instead.
  Note: You can use "dune upgrade" to convert your project to dune.
  $ OCAMLPATH=jbuild-driver/_build/install/default/lib dune build --root use-external-driver driveruser.cma
  Entering directory 'use-external-driver'
           ppx driveruser.pp.ml
  .ppx/631757a4a4789e0bd29628f7a73480f7/ppx.exe
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
