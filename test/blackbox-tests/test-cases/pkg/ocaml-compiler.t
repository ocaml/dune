Setting the compiler in the lock directory

We need some data for ocamlc -config

  $ mkdir stdlib && touch stdlib/Makefile.config
  $ . ./helpers.sh
  $ cat >ocaml.config <<EOF
  > version: 4.14.1
  > standard_library_default: $PWD/stdlib
  > standard_library: $PWD/stdlib
  > ccomp_type: cc
  > c_compiler: clang
  > ocamlc_cflags: -O2 -fno-strict-aliasing -fwrapv -pthread
  > ocamlc_cppflags: -D_FILE_OFFSET_BITS=64
  > ocamlopt_cflags: -O2 -fno-strict-aliasing -fwrapv -pthread
  > ocamlopt_cppflags: -D_FILE_OFFSET_BITS=64
  > bytecomp_c_compiler: clang -O2 -fno-strict-aliasing -fwrapv -pthread   -D_FILE_OFFSET_BITS=64
  > native_c_compiler: clang -O2 -fno-strict-aliasing -fwrapv -pthread   -D_FILE_OFFSET_BITS=64
  > bytecomp_c_libraries: -lm  -lpthread
  > native_c_libraries: -lm
  > native_pack_linker: ld -r -o
  > architecture: amd64
  > model: default
  > int_size: 63
  > word_size: 64
  > system: macosx
  > asm: fake
  > asm_cfi_supported: true
  > with_frame_pointers: false
  > ext_exe:
  > ext_obj: .o
  > ext_asm: .s
  > ext_lib: .a
  > ext_dll: .so
  > os_type: Unix
  > default_executable_name: a.out
  > systhread_supported: true
  > host: x86_64-apple-darwin
  > target: x86_64-apple-darwin
  > flambda: false
  > safe_string: true
  > default_safe_string: true
  > flat_float_array: true
  > function_sections: false
  > afl_instrument: false
  > windows_unicode: false
  > supports_shared_libraries: true
  > naked_pointers: true
  > exec_magic_number: Caml1999X031
  > cmi_magic_number: Caml1999I031
  > cmo_magic_number: Caml1999O031
  > cma_magic_number: Caml1999A031
  > cmx_magic_number: Caml1999Y031
  > cmxa_magic_number: Caml1999Z031
  > ast_impl_magic_number: Caml1999M031
  > ast_intf_magic_number: Caml1999N031
  > cmxs_magic_number: Caml1999D031
  > cmt_magic_number: Caml1999T031
  > linear_magic_number: Caml1999L031
  > EOF

This is the directory for our fake OCaml the test shouldn't observe. We can't
use the system OCaml for this.

  $ mkdir shadowsystemocaml
  $ cat >shadowsystemocaml/ocamlc <<EOF
  > #!/usr/bin/env sh
  > cat ocaml.config
  > EOF
  $ chmod +x shadowsystemocaml/ocamlc
  $ export PATH="$PWD/shadowsystemocaml:$PATH"

Now we finally make the OCaml package for testing through the lock file:

  $ mkdir dune.lock
  $ cat >dune.lock/lock.dune <<EOF
  > (lang package 0.1)
  > (ocaml mycaml)
  > EOF
  $ cat >dune.lock/mycaml.pkg <<EOF
  > (version 0.0.1)
  > (source (copy $PWD/mycamlsources))
  > (build
  >  (system "\| cat >mycaml.install <<EOF
  >          "\| bin: [ "ocamlc" ]
  >          "\| EOF
  > ))
  > EOF

  $ cat >dune-project <<EOF
  > (lang dune 3.11)
  > EOF

  $ mkdir mycamlsources
  $ cat >mycamlsources/ocamlc <<EOF
  > #!/usr/bin/env sh
  > cat $PWD/ocaml.config
  > EOF
  $ chmod +x mycamlsources/ocamlc

  $ cat >dune <<EOF
  > (dirs :standard \ mycamlsources shadowsystemocaml stdlib)
  > (rule
  >  (alias foo)
  >  (action (system "command -v ocamlc foo")))
  > EOF

This should display the ocaml from the lock file rather than shadowsystemocaml

  $ dune build @foo
  $TESTCASE_ROOT/_build/_private/default/.pkg/mycaml/target/bin/ocamlc
