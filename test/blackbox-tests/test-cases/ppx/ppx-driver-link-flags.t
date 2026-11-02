The ppx_driver_link_flags config field adds flags to the link of ppx drivers,
and only to ppx drivers.

  $ export DUNE_TRACE="process"
  $ cat > dune-project <<EOF
  > (lang dune 3.26)
  > (package (name foo))
  > EOF
  $ cat > dune <<EOF
  > (library
  >  (name driver)
  >  (public_name foo.driver)
  >  (modules driver)
  >  (ppx.driver (main Driver.main)))
  > (library
  >  (name ppx)
  >  (public_name foo.ppx)
  >  (kind ppx_rewriter)
  >  (modules ())
  >  (libraries driver))
  > (executable
  >  (name main)
  >  (modules main)
  >  (preprocess (pps ppx)))
  > EOF
  $ cat > driver.ml <<EOF
  > let main () =
  >   let out = ref "" in
  >   let args =
  >     [ ("-o", Arg.Set_string out, "")
  >     ; ("--impl", Arg.Set_string (ref ""), "")
  >     ; ("--as-ppx", Arg.Set (ref false), "")
  >     ; ("--cookie", Arg.String ignore, "")
  >     ]
  >   in
  >   Arg.parse (Arg.align args) ignore "";
  >   close_out (open_out !out)
  > EOF
  $ touch main.ml

  $ link_flags () {
  >   dune trace cat | jq_dune -c '
  >     processes
  >     | .args
  >     | select(targets | any(endswith("/ppx.exe") or endswith("/main.exe")))
  >     | [ (.target_files[] | select(endswith(".exe")) | basename)
  >       , (.process_args | map(select(. == "-linkall"))) ]
  >   '
  > }

By default, no extra flag is passed:

  $ dune build ./main.exe
  $ link_flags
  ["ppx.exe",[]]
  ["main.exe",[]]

The field can be set in the workspace file. It applies to the driver and not
to the executable:

  $ cat > dune-workspace <<EOF
  > (lang dune 3.26)
  > (ppx_driver_link_flags (-linkall))
  > EOF
  $ dune build ./main.exe
  $ link_flags
  ["ppx.exe",["-linkall"]]

Removing the field links the driver again without the flags:

  $ rm dune-workspace
  $ dune build ./main.exe
  $ link_flags
  ["ppx.exe",[]]

It can be set in the config file too:

  $ cat > config <<EOF
  > (lang dune 3.26)
  > (ppx_driver_link_flags (-linkall))
  > EOF
  $ dune build --config-file config ./main.exe
  $ link_flags
  ["ppx.exe",["-linkall"]]

A value in the workspace file replaces the one from the config file:

  $ cat > dune-workspace <<EOF
  > (lang dune 3.26)
  > (ppx_driver_link_flags ())
  > EOF
  $ dune build --config-file config ./main.exe
  $ link_flags
  ["ppx.exe",[]]

The driver installed for a ppx_rewriter library gets the flags too:

  $ rm dune-workspace
  $ dune build --config-file config @install
  $ link_flags
  ["ppx.exe",["-linkall"]]
  $ test -e _build/install/default/lib/foo/ppx/ppx.exe

The field is only available since version 3.26 of the dune language:

  $ cat > dune-workspace <<EOF
  > (lang dune 3.25)
  > (ppx_driver_link_flags (-linkall))
  > EOF
  $ dune build ./main.exe
  File "dune-workspace", line 2, characters 0-34:
  2 | (ppx_driver_link_flags (-linkall))
      ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
  Error: 'ppx_driver_link_flags' is only available since version 3.26 of the
  dune language. Please update your dune config file to have (lang dune 3.26).
  [1]
