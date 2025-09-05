Test using installed drivers

  $ dune build --root driver @install
  Entering directory 'driver'
  Leaving directory 'driver'
  $ OCAMLPATH=driver/_build/install/default/lib dune build --root use-external-driver driveruser.cma
  Entering directory 'use-external-driver'
  .ppx/d1c2c0cf2b61b462226db18f72732070/ppx.exe
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
  Leaving directory 'use-external-driver'
  [1]

  $ OCAMLPATH=driver/_build/install/default/lib dune build --root replaces driveruser.cma
  Entering directory 'replaces'
  replacesdriver
  .ppx/5c2c5102d9f019e3d5eee93d6abe2029/ppx.exe
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
  Leaving directory 'replaces'
  [1]

  $ OCAMLPATH=driver/_build/install/default/lib dune build --root driver-replaces @install
  Entering directory 'driver-replaces'
  Leaving directory 'driver-replaces'
  $ OCAMLPATH=driver/_build/install/default/lib:driver-replaces/_build/install/default/lib dune build --root replaces-external driveruser.cma
  Entering directory 'replaces-external'
  replacesdriver
  .ppx/5c2c5102d9f019e3d5eee93d6abe2029/ppx.exe
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
  Leaving directory 'replaces-external'
  [1]
