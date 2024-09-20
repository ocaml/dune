Test enabled_if with 'env' variable.

  $ cat > dune-project <<EOF
  > (lang dune 3.14)
  > (name dune-test)
  > (package
  >  (name dune-test))
  > EOF
 
  $ cat > dune <<EOF
  > (executable
  >  (name main)
  >  (public_name dune_test)
  >  (modules main)
  >  (package dune-test)
  >  (modes exe))
  > (executable
  >  (name main_2)
  >  (public_name dune_test_2)
  >  (enabled_if (= enabled %{env:MYVAR=disabled}))
  >  (modules main_2)
  >  (package dune-test)
  >  (modes exe))
  > EOF

  $ MYVAR=disabled dune exec -- dune_test
  File "dune", line 10, characters 24-45:
  10 |  (enabled_if (= enabled %{env:MYVAR=disabled}))
                               ^^^^^^^^^^^^^^^^^^^^^
  Error: %{env:..} is only available since version 3.15 of the dune language.
  Please update your dune-project file to have (lang dune 3.15).
  [1]

  $ cat > dune-project <<EOF
  > (lang dune 3.15)
  > (name dune-test)
  > (package
  >  (name dune-test))
  > EOF

  $ cat > main.ml <<EOF
  > let () = print_string "Hello world"
  > EOF

  $ MYVAR=disabled dune exec -- dune_test
  Hello world
  $ MYVAR=enabled dune exec -- dune_test
  File "dune", line 11, characters 10-16:
  11 |  (modules main_2)
                 ^^^^^^
  Error: Module Main_2 doesn't exist.
  [1]
