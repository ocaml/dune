Setting the compiler in the lock directory

We need some data for ocamlc -config

  $ mkdir stdlib && touch stdlib/Makefile.config

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

  $ make_lockdir
  $ cat >> ${source_lock_dir}/lock.dune <<EOF
  > (ocaml mycaml)
  > EOF
  $ make_lockpkg mycaml <<EOF
  > (version 0.0.1)
  > (source (copy $PWD/mycamlsources))
  > (build
  >  (system "\| cat >mycaml.install <<EOF
  >          "\| bin: [ "ocamlc" ]
  >          "\| EOF
  > ))
  > EOF

  $ make_dune_project 3.11

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

  $ DUNE_CONFIG__SYS_OCAML_VERSION=4.14.1 dune build @foo 2>&1 | censor
  $PWD/_build/default/.lockfile/pkg/mycaml/.opam/mycaml/target/bin/ocamlc

The compiler's auxiliary tools can live in a transitive compiler package, as
with the ocaml / ocaml-base-compiler / ocaml-compiler split. They must remain
available to both scoped workspace packages and mounted projects, without an
ambient OCaml installation.

  $ cat >mycamlsources/ocamllex <<'EOF'
  > #!/bin/sh
  > while test "$1" != -o; do shift; done
  > printf 'let source = "managed lexer"\n' > "$2"
  > EOF
  $ cat >mycamlsources/ocamlyacc <<'EOF'
  > #!/bin/sh
  > printf 'let source = "managed parser"\n' > "${1%.mly}.ml"
  > printf 'val source : string\n' > "${1%.mly}.mli"
  > EOF
  $ cat >mycamlsources/ocaml <<'EOF'
  > #!/bin/sh
  > echo managed toplevel
  > EOF
  $ cp mycamlsources/ocaml mycamlsources/compiler-private-tool
  $ chmod +x mycamlsources/ocamllex mycamlsources/ocamlyacc \
  >   mycamlsources/ocaml mycamlsources/compiler-private-tool
  $ make_lockpkg mycaml-tools <<EOF
  > (version 0.0.1)
  > (source (copy $PWD/mycamlsources))
  > (build
  >  (write-file mycaml-tools.install
  >   "bin: [ \"ocaml\" \"ocamllex\" \"ocamlyacc\" \"compiler-private-tool\" ]"))
  > EOF
  $ make_lockpkg mycaml <<EOF
  > (version 0.0.1)
  > (depends mycaml-tools)
  > (source (copy $PWD/mycamlsources))
  > (build (write-file mycaml.install "bin: [ \"ocamlc\" ]"))
  > EOF
  $ make_lockpkg unrelated <<'EOF'
  > (version 1)
  > (build (run false))
  > EOF

Keep only the tools needed to build the fake compiler on PATH.

  $ mkdir _path
  $ for tool in dune sh bash cat mkdir cp chmod grep; do
  >   ln -s "$(command -v "$tool")" "_path/$tool"
  > done
  $ export PATH="$PWD/_path"
  $ cat >dune-project <<'EOF'
  > (lang dune 3.25)
  > (package (name main) (allow_empty) (dir .) (depends mycaml))
  > EOF
  $ cat >dune <<'EOF'
  > (dirs :standard \ mycamlsources shadowsystemocaml stdlib)
  > (ocamllex lexer)
  > (ocamlyacc parser)
  > (rule
  >  (with-stdout-to toplevel-output (run ocaml)))
  > (rule
  >  (with-stdout-to private-available
  >   (echo %{bin-available:compiler-private-tool})))
  > EOF
  $ echo 'rule token = parse | eof { () }' >lexer.mll
  $ cat >parser.mly <<'EOF'
  > %token EOF
  > %start main
  > %type <unit> main
  > %%
  > main: EOF { () }
  > EOF

The narrowed lookup currently loses all three tools:

  $ dune build lexer.ml parser.ml toplevel-output private-available \
  >   >diagnostic 2>&1
  [1]
  $ grep '^Error:' diagnostic
  Error: Program ocamllex not found in the tree or in PATH
  Error: Program ocamlyacc not found in the tree or in PATH
  Error: Program ocaml not found in the tree or in PATH
  $ cat _build/default/private-available
  false
  $ test ! -e _build/default/.lockfile/pkg/unrelated/.opam/unrelated/target

Mounted projects use the same selected compiler, even when they only depend on
the metadata package rather than the package installing its tools.

  $ mkdir _native
  $ cp dune lexer.mll parser.mly _native/
  $ cat >_native/dune-project <<'EOF'
  > (lang dune 3.25)
  > (package (name native))
  > EOF
  $ echo '(library (public_name native))' >>_native/dune
  $ make_lockpkg native <<EOF
  > (version 1)
  > (depends mycaml)
  > (source (copy $PWD/_native))
  > (build (run false))
  > EOF
  $ native_dir=_build/default/.lockfile/pkg/native
  $ dune build "$native_dir/lexer.ml" "$native_dir/parser.ml" \
  >   "$native_dir/toplevel-output" "$native_dir/private-available" \
  >   >diagnostic 2>&1
  [1]
  $ grep '^Error:' diagnostic
  Error: Program ocamllex not found in the tree or in PATH
  Error: Program ocamlyacc not found in the tree or in PATH
  Error: Program ocaml not found in the tree or in PATH
  $ cat "$native_dir/private-available"
  false
  $ test ! -e _build/default/.lockfile/pkg/native/.opam/native/target
  $ test ! -e _build/default/.lockfile/pkg/unrelated/.opam/unrelated/target
