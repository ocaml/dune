Build mixed-language archives and stubs with per-language and shared flags.

Setup
=====

  $ make_dune_project 3.26
  $ echo '(use_standard_c_and_cxx_flags true)' >> dune-project
  $ cat > env <<'EOF'
  > (env
  >  (_
  >   (c_flags :standard -DC_ONLY)
  >   (cxx_flags :standard -DCXX_ONLY)))
  > EOF
  $ cat > foo_c.c <<'EOF'
  > #include <caml/mlvalues.h>
  > #if !defined(C_ONLY) || defined(CXX_ONLY)
  > #error Incorrect C flags
  > #endif
  > #ifndef STUB_FLAGS
  > #error Missing explicit stub flags
  > #endif
  > CAMLprim value hello_c(value unit) {
  >   (void)unit;
  >   return Val_int(40);
  > }
  > EOF
  $ cat > foo_cpp.cpp <<'EOF'
  > #ifndef __cplusplus
  > #error Not a C++ compiler
  > #endif
  > #if !defined(CXX_ONLY) || defined(C_ONLY)
  > #error Incorrect C++ flags
  > #endif
  > #ifndef STUB_FLAGS
  > #error Missing explicit stub flags
  > #endif
  > #include <caml/mlvalues.h>
  > extern "C" CAMLprim value hello_cpp(value unit) {
  >   (void)unit;
  >   return Val_int(2);
  > }
  > EOF
  $ cat > main.ml <<'EOF'
  > external hello_c : unit -> int = "hello_c"
  > external hello_cpp : unit -> int = "hello_cpp"
  > let () = Printf.printf "%d\n" (hello_c () + hello_cpp ())
  > EOF

Mixed static archive
====================

A single foreign archive can contain both C and C++ sources.

  $ cat > dune <<'EOF'
  > (foreign_library
  >  (archive_name mixed)
  >  (languages c cxx)
  >  (names :standard)
  >  (flags :standard -DSTUB_FLAGS))
  > (executable
  >  (name main)
  >  (foreign_archives mixed))
  > (include env)
  > EOF
  $ dune exec ./main.exe
  42

Executable stubs and compilation database
=========================================

The C++ stub now requires the C++ runtime. Both sources also check that their
compiler receives the shared stub flags and only its own language's defaults.

  $ cat >> foo_cpp.cpp <<'EOF'
  > #include <string>
  > extern "C" CAMLprim value string_length(value s) {
  >   return Val_int(std::string(String_val(s)).size());
  > }
  > EOF
  $ cat > main.ml <<'EOF'
  > external hello_c : unit -> int = "hello_c"
  > external string_length : string -> int = "string_length"
  > let () = Printf.printf "%d\n" (hello_c () + string_length "hi")
  > EOF
  $ touch ignored.c ignored.cpp
  $ cat > dune <<'EOF'
  > (executable
  >  (name main)
  >  (foreign_stubs
  >   (languages c cxx)
  >   (names :standard \ ignored)
  >   (flags :standard -DSTUB_FLAGS)))
  > (include env)
  > EOF
  $ dune exec ./main.exe
  42
  $ dune build @check
  $ jq -c 'sort_by(.file)[] | {file, flags: [.arguments[] |
  >   select(IN("-DC_ONLY", "-DCXX_ONLY", "-DSTUB_FLAGS"))]}' \
  >   compile_commands.json
  {"file":"foo_c.c","flags":["-DC_ONLY","-DSTUB_FLAGS"]}
  {"file":"foo_cpp.cpp","flags":["-DCXX_ONLY","-DSTUB_FLAGS"]}

Subdirectory extensions
=======================

Other C++ extensions work with the same flags and linking.

  $ mkdir src
  $ mv foo_cpp.cpp src/foo_cpp.cc
  $ echo '(include_subdirs unqualified)' >> dune
  $ dune exec ./main.exe
  42
  $ mv src/foo_cpp.cc src/foo_cpp.cxx
  $ dune exec ./main.exe
  42

C++ linker defaults with C-only stubs
=====================================

Declaring cxx still requests its linker defaults when only C stubs are selected.

  $ cat > dune <<'EOF'
  > (include_subdirs unqualified)
  > (include env)
  > (foreign_library
  >  (archive_name cpp)
  >  (language cxx)
  >  (names foo_cpp)
  >  (flags :standard -DSTUB_FLAGS))
  > (executable
  >  (name main)
  >  (foreign_archives cpp)
  >  (foreign_stubs
  >   (languages c cxx)
  >   (names foo_c)
  >   (flags :standard -DSTUB_FLAGS)))
  > EOF
  $ dune exec ./main.exe
  42

Generated library stubs
=======================

The same mixed stubs work in libraries, natively and in custom bytecode.

  $ cat > bindings.ml <<'EOF'
  > external hello_c : unit -> int = "hello_c"
  > external string_length : string -> int = "string_length"
  > let run () = Printf.printf "%d\n" (hello_c () + string_length "hi")
  > EOF
  $ echo 'let () = Bindings.run ()' > main.ml
  $ mv foo_c.c foo_c.c.in
  $ mv src/foo_cpp.cxx src/foo_cpp.cxx.in
  $ echo '(rule (copy foo_cpp.cxx.in foo_cpp.cxx))' > src/dune
  $ cat > dune <<'EOF'
  > (include_subdirs unqualified)
  > (rule (copy foo_c.c.in foo_c.c))
  > (library
  >  (name bindings)
  >  (modules bindings)
  >  (foreign_stubs
  >   (languages c cxx)
  >   (names :standard \ ignored)
  >   (flags :standard -DSTUB_FLAGS)))
  > (executable
  >  (name main)
  >  (modules main)
  >  (libraries bindings)
  >  (modes exe byte_complete))
  > (include env)
  > EOF
  $ dune exec ./main.exe
  42
  $ dune exec ./main.bc.exe
  42

Mixed shared archive
====================

Pure bytecode loads the archive's shared library. This checks that the archive
links the C++ runtime, independently of executable linker defaults.

  $ cat > dune <<'EOF'
  > (include_subdirs unqualified)
  > (rule (copy foo_c.c.in foo_c.c))
  > (foreign_library
  >  (archive_name mixed)
  >  (languages c cxx)
  >  (names :standard \ ignored)
  >  (flags :standard -DSTUB_FLAGS))
  > (library
  >  (name bindings)
  >  (modules bindings)
  >  (foreign_archives mixed))
  > (executable
  >  (name main)
  >  (modules main)
  >  (libraries bindings)
  >  (modes byte))
  > (include env)
  > EOF
  $ dune build main.bc
  $ (cd _build/default && ocamlrun -I . main.bc)
  42
