Using binary operators for dependencies
---------------------------------------

Not supported before 2.1:

  $ cat > dune-project <<EOF
  > (lang dune 2.0)
  > (name foo)
  > (generate_opam_files true)
  > (package
  >  (name foo)
  >  (depends (conf-libX11 (<> :os win32))))
  > EOF

  $ dune build @install
  File "dune-project", line 6, characters 23-37:
  6 |  (depends (conf-libX11 (<> :os win32))))
                             ^^^^^^^^^^^^^^
  Error: Passing two arguments to <> is only available since version 2.1 of the
  dune language. Please update your dune-project file to have (lang dune 2.1).
  [1]

Supported since 2.1:

  $ cat > dune-project <<EOF
  > (lang dune 2.1)
  > (name foo)
  > (generate_opam_files true)
  > (package
  >  (name foo)
  >  (depends (conf-libX11 (<> :os win32))))
  > EOF

  $ dune build @install
  $ grep conf-libX11 foo.opam
    "conf-libX11" {os != "win32"}
