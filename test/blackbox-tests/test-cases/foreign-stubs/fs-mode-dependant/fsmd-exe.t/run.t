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
