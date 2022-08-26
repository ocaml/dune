##############
Project toggle
##############

Without the toggle we get an error message for using the new mode subfield
  $ cat >dune-project <<EOF
  > (lang dune 3.5)
  > EOF

  $ dune build 2>&1 | head -n 6
  File "dune", line 11, characters 3-16:
  11 |    (mode native)
          ^^^^^^^^^^^^^
  Error: The field "mode"
                 must be enabled with the "allow_mode_specific_stubs" option in
  the "dune-project" file.

But the toggle only exists in Dune 3.5
  $ cat >dune-project <<EOF
  > (lang dune 3.1)
  > (allow_mode_specific_stubs)
  > EOF

  $ dune build
  File "dune-project", line 2, characters 0-27:
  2 | (allow_mode_specific_stubs)
      ^^^^^^^^^^^^^^^^^^^^^^^^^^^
  Error: 'allow_mode_specific_stubs' is only available since version 3.5 of the
  dune language. Please update your dune-project file to have (lang dune 3.5).
  [1]

With Dune 3.5 no error is displayed
  $ cat >dune-project <<EOF
  > (lang dune 3.5)
  > (allow_mode_specific_stubs)
  > EOF

  $ dune build
  $ dune clean

###########
Executables
###########

Native executables should output 1, others 0 if they have mode-dependent stubs
  $ dune exec ./stubs_exe.exe
  Byte (0) or native (1) ? 1

  $ dune exec ./stubs_exe.bc.exe
  Byte (0) or native (1) ? 0

But stubs_same_exe which does not have mode dependent stubs and should
always output 0
  $ dune exec ./stubs_same_exe.exe
  Byte (0) and native (0) ? 0

  $ dune exec ./stubs_same_exe.bc.exe
  Byte (0) and native (0) ? 0

There should be two different object files for the mode-dependent stub `c_stub`
but only one for the non-mode-dependent stub `c_stub_same`
  $ ls _build/default/*.o
  _build/default/c_stubs_byte.o
  _build/default/c_stubs_native.o
  _build/default/c_stubs_same.o

  $ dune clean

#########
Libraries
#########



  $ dune exec ./stubs_lib.exe
  Byte (0) or native (1) ? 1

  $ dune exec ./stubs_lib.bc.exe
  Byte (0) or native (1) ? 0

 
  $ dune clean

Now we try will being in the sandbox
  $ cat >sdune <<'EOF'
  > #!/usr/bin/env bash
  > DUNE_SANDBOX=symlink dune "$@"
  > EOF
  $ chmod +x sdune

  $ ./sdune exec ./stubs_exe.exe
  Byte (0) or native (1) ? 1

  $ ./sdune exec ./stubs_exe.bc.exe
  Byte (0) or native (1) ? 0

  $ ./sdune exec ./stubs_lib.exe
  Byte (0) or native (1) ? 1

  $ ./sdune exec ./stubs_lib.bc.exe
  Byte (0) or native (1) ? 0
