  $ dune exec ./discover.exe
  run: /Users/rgrinberg/.opam/4.06.1/bin/ocamlc.opt -config
  -> process exited with code 0
  -> stdout:
   | version: 4.06.1
   | standard_library_default: /Users/rgrinberg/.opam/4.06.1/lib/ocaml
   | standard_library: /Users/rgrinberg/.opam/4.06.1/lib/ocaml
   | standard_runtime: /Users/rgrinberg/.opam/4.06.1/bin/ocamlrun
   | ccomp_type: cc
   | c_compiler: cc
   | ocamlc_cflags: -O2 -fno-strict-aliasing -fwrapv 
   | ocamlc_cppflags: -D_FILE_OFFSET_BITS=64 -D_REENTRANT
   | ocamlopt_cflags: -O2 -fno-strict-aliasing -fwrapv
   | ocamlopt_cppflags: -D_FILE_OFFSET_BITS=64 -D_REENTRANT
   | bytecomp_c_compiler: cc -O2 -fno-strict-aliasing -fwrapv  -D_FILE_OFFSET_BITS=64 -D_REENTRANT
   | native_c_compiler: cc -O2 -fno-strict-aliasing -fwrapv -D_FILE_OFFSET_BITS=64 -D_REENTRANT
   | bytecomp_c_libraries: -lcurses -lpthread                  
   | native_c_libraries: 
   | native_pack_linker: ld -r -o 
   | ranlib: ranlib
   | cc_profile: -pg
   | architecture: amd64
   | model: default
   | int_size: 63
   | word_size: 64
   | system: macosx
   | asm: clang -arch x86_64 -Wno-trigraphs -c
   | asm_cfi_supported: true
   | with_frame_pointers: false
   | ext_exe: 
   | ext_obj: $ext_obj
   | ext_asm: $ext_asm
   | ext_lib: $ext_lib
   | ext_dll: $ext_dll
   | os_type: Unix
   | default_executable_name: a.out
   | systhread_supported: true
   | host: x86_64-apple-darwin17.5.0
   | target: x86_64-apple-darwin17.5.0
   | profiling: true
   | flambda: false
   | spacetime: false
   | safe_string: false
   | default_safe_string: true
   | flat_float_array: true
   | afl_instrument: false
   | windows_unicode: false
   | exec_magic_number: Caml1999X011
   | cmi_magic_number: Caml1999I022
   | cmo_magic_number: Caml1999O022
   | cma_magic_number: Caml1999A022
   | cmx_magic_number: Caml1999Y022
   | cmxa_magic_number: Caml1999Z022
   | ast_impl_magic_number: Caml1999M022
   | ast_intf_magic_number: Caml1999N022
   | cmxs_magic_number: Caml1999D022
   | cmt_magic_number: Caml1999T022
  -> stderr:
  Fatal error: exception Stdune__Exn.Code_error(_)
  [2]
