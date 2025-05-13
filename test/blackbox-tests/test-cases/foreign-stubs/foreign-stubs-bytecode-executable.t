----------------------------------------------------------------------------------
Testing the building of bytecode executables with foreign archives.

  $ echo "(lang dune 2.0)" > dune-project
  $ ./sandboxed.sh

----------------------------------------------------------------------------------
* Fails to build a pure bytecode executable with a foreign archive

  $ cat >dune <<EOF
  > (executable
  >  (modes byte)
  >  (name main)
  >  (modules main)
  >  (foreign_archives time))
  > (foreign_library
  >  (archive_name time)
  >  (language c)
  >  (names time))
  > EOF

  $ cat >time.c <<EOF
  > #include <caml/mlvalues.h>
  > value current_time(value unit) { return Val_int(1345); }
  > EOF

  $ cat >main.ml <<EOF
  > external current_time : unit -> int = "current_time"
  > let () = Printf.printf "clock = %d" (current_time ())
  > EOF

  $ dune clean
  $ dune exec ./main.bc
  File "dune", lines 1-5, characters 0-80:
  1 | (executable
  2 |  (modes byte)
  3 |  (name main)
  4 |  (modules main)
  5 |  (foreign_archives time))
  Error: Pure bytecode executables cannot contain foreign archives.
  Hint: If you only need to build a native executable use "(modes exe)".
  [1]

----------------------------------------------------------------------------------
* Build a bytecode executable by statically linking in a foreign archive when the
setting [disable_dynamically_linked_foreign_archives] is [true] in the workspace

  $ cat >dune-workspace <<EOF
  > (lang dune 2.0)
  > (context
  >   (default (disable_dynamically_linked_foreign_archives true)))
  > EOF

  $ dune clean
  $ dune exec ./main.bc
  clock = 1345

----------------------------------------------------------------------------------
* Make sure no rules are generated for foreign dynamically linked archives

  $ dune build _build/default/dlltime.so
  Error: Don't know how to build _build/default/dlltime.so
  [1]

'
----------------------------------------------------------------------------------
* Fails to install a library with foreign stubs when a [dll*.so] rule is missing

  $ cat >dune-project <<EOF
  > (lang dune 1.11)
  > (package
  >   (name foo))
  > EOF

  $ cat >dune-workspace <<EOF
  > (lang dune 2.0)
  > (context
  >   (default (disable_dynamically_linked_foreign_archives false)))
  > EOF

  $ cat >dune <<EOF
  > (library
  >  (public_name foo.clock)
  >  (name clock)
  >  (modules clock)
  >  (self_build_stubs_archive (time)))
  > (rule
  >  (targets time%{ext_obj})
  >  (deps time.c)
  >  (action (run %{ocaml-config:c_compiler} -c -I %{ocaml-config:standard_library} -o %{targets} %{deps})))
  > (rule
  >  (targets libtime_stubs.a)
  >  (deps time%{ext_obj})
  >  (action (run ar rcs %{targets} %{deps})))
  > EOF

  $ cat >clock.ml <<EOF
  > external current_time : unit -> int = "current_time"
  > let clock = current_time ()
  > EOF

  $ cat >clock.mli <<EOF
  > val clock : int
  > EOF

  $ dune clean
  $ dune build @install
  File "dune", lines 1-5, characters 0-100:
  1 | (library
  2 |  (public_name foo.clock)
  3 |  (name clock)
  4 |  (modules clock)
  5 |  (self_build_stubs_archive (time)))
  Error: No rule found for dlltime_stubs.so
  [1]

----------------------------------------------------------------------------------
* Succeeds to install a library with foreign stubs when a [dll*.so] rule is missing
but the setting [disable_dynamically_linked_foreign_archives] is [true] in the workspace

  $ cat >dune-workspace <<EOF
  > (lang dune 2.0)
  > (context
  >   (default (disable_dynamically_linked_foreign_archives true)))
  > EOF

  $ dune clean
  $ dune build @install

