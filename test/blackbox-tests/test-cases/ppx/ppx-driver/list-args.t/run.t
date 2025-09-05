Test the argument syntax with list expansion allowed (dune > 3.2)

  $ dune build
  .ppx/7e2e4f08e99331b1c723b81d3aa870ba/ppx.exe
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
  File "dune", lines 23-24, characters 3-161:
  23 |    (pps -arg1 driver_print_args ppx_with_cookies_print_args -arg2 -arg3=%{env:AMERICA=undefined} --
  24 |     -foo bar %{env:ENGLAND=undefined} %{read-lines:ppx-args})))
  Error: Rule failed to generate the following targets:
  - test_ppx_args.pp.ml
  [1]
