Explicit language lists work without assembly support or language inference.

  $ make_dune_project 3.26
  $ echo '(use_standard_c_and_cxx_flags true)' >> dune-project
  $ cat > foo_c.c <<'EOF'
  > #include <caml/mlvalues.h>
  > #if !defined(C_ONLY) || defined(CXX_ONLY)
  > #error Incorrect C flags
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

A single foreign archive can contain both C and C++ sources.

  $ cat > dune <<'EOF'
  > (foreign_library
  >  (archive_name mixed)
  >  (languages c cxx)
  >  (names foo_c foo_cpp))
  > (executable
  >  (name main)
  >  (foreign_archives mixed))
  > (env
  >  (_
  >   (c_flags :standard -DC_ONLY)
  >   (cxx_flags :standard -DCXX_ONLY)))
  > EOF
  $ dune exec ./main.exe
  42

The same language list works for foreign stubs, including C++ runtime linking.

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
  $ cat > dune <<'EOF'
  > (executable
  >  (name main)
  >  (foreign_stubs
  >   (languages c cxx)
  >   (names foo_c foo_cpp)))
  > (env
  >  (_
  >   (c_flags :standard -DC_ONLY)
  >   (cxx_flags :standard -DCXX_ONLY)))
  > EOF
  $ dune exec ./main.exe
  42
  $ dune build @check
  $ jq -r '.[].file' compile_commands.json | sort
  foo_c.c
  foo_cpp.cpp

Other C++ extensions work in subdirectories with the same flags and linking.

  $ mkdir src
  $ mv foo_cpp.cpp src/foo_cpp.cc
  $ echo '(include_subdirs unqualified)' >> dune
  $ dune exec ./main.exe
  42
  $ mv src/foo_cpp.cc src/foo_cpp.cxx
  $ dune exec ./main.exe
  42

Sources with the same object name are still rejected.

  $ touch foo_c.cpp
  $ dune build
  File "dune", line 5, characters 9-14:
  5 |   (names foo_c foo_cpp)))
               ^^^^^
  Error: Multiple sources map to the same object name "foo_c":
  - foo_c.c
  - foo_c.cpp
  This is not allowed; please rename them or remove "foo_c" from object names.
  Hint: You can also avoid the name clash by placing the objects into different
  foreign archives and building them in different directories. Foreign archives
  can be defined using the (foreign_library ...) stanza.
  [1]
  $ rm foo_c.cpp

The new field is versioned.

  $ make_dune_project 3.25
  $ dune build
  File "dune", line 4, characters 2-19:
  4 |   (languages c cxx)
        ^^^^^^^^^^^^^^^^^
  Error: 'languages' is only available since version 3.26 of the dune language.
  Please update your dune-project file to have (lang dune 3.26).
  [1]
