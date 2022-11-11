  $ cat >stubs_exe.ml <<EOF
  > external stub_byte_or_native : unit -> int = "caml_b_or_n"
  > let () = 
  >   Printf.printf "Byte (0) or native (1) ? %i\n" (stub_byte_or_native ())
  > EOF


We can have one stub that is mode-dependent:
  $ cat >dune <<EOF
  > (executable
  >  (modes native byte_complete)
  >  (modules stubs_exe)
  >  (name stubs_exe)
  >  (foreign_stubs
  >   (mode native)
  >   (language c)
  >   (flags :standard -DNATIVE_CODE)
  >   (names c_stubs))
  >  (foreign_stubs
  >   (mode byte)
  >   (language c)
  >   (flags :standard)
  >   (names c_stubs)))
  > EOF

  $ dune exec ./stubs_exe.exe
  Byte (0) or native (1) ? 1

  $ dune exec ./stubs_exe.bc.exe
  Byte (0) or native (1) ? 0

If one of the two is not specified, it will be used for both byte and native mode
and then a duplicated symbol error will happen
  $ cat >dune <<EOF
  > (executable
  >  (modes native byte_complete)
  >  (modules stubs_exe)
  >  (name stubs_exe)
  >  (foreign_stubs
  >   (language c)
  >   (flags :standard -DNATIVE_CODE)
  >   (names c_stubs))
  >  (foreign_stubs
  >   (mode byte)
  >   (language c)
  >   (flags :standard)
  >   (names c_stubs)))
  > EOF

  $ dune exec ./stubs_exe.exe
  Byte (0) or native (1) ? 1

FIXME: we could detect this earlier and display a better error message
  $ dune exec ./stubs_exe.bc.exe 2>&1 | grep -q 'duplicate symbol\|multiple definition' 

But two foreign stubs for the same file and mode is an error:
  $ cat >dune <<EOF
  > (executable
  >  (modes native byte_complete)
  >  (modules stubs_exe)
  >  (name stubs_exe)
  >  (foreign_stubs
  >   (mode byte)
  >   (language c)
  >   (flags :standard -DNATIVE_CODE)
  >   (names c_stubs))
  >  (foreign_stubs
  >   (mode byte)
  >   (language c)
  >   (flags :standard)
  >   (names c_stubs)))
  > EOF

  $ dune exec ./stubs_exe.exe
  File "dune", line 9, characters 9-16:
  9 |   (names c_stubs))
               ^^^^^^^
  Error: Multiple sources map to the same object name "c_stubs" for mode byte:
  - c_stubs.c
  - c_stubs.c
  This is not allowed; please rename them or remove "c_stubs" from object
  names.
  Hint: You may be missing a mode field that would restrict this stub to some
  specific mode.
  Hint: You can also avoid the name clash by placing the objects into different
  foreign archives and building them in different directories. Foreign archives
  can be defined using the (foreign_library ...) stanza.
  [1]

  $ dune exec ./stubs_exe.bc.exe
  File "dune", line 9, characters 9-16:
  9 |   (names c_stubs))
               ^^^^^^^
  Error: Multiple sources map to the same object name "c_stubs" for mode byte:
  - c_stubs.c
  - c_stubs.c
  This is not allowed; please rename them or remove "c_stubs" from object
  names.
  Hint: You may be missing a mode field that would restrict this stub to some
  specific mode.
  Hint: You can also avoid the name clash by placing the objects into different
  foreign archives and building them in different directories. Foreign archives
  can be defined using the (foreign_library ...) stanza.
  [1]

We can have some mode-dependent stubs and some non-dependent other stubs
  $ cat >dune <<EOF
  > (executable
  >  (modes native byte_complete)
  >  (modules stubs_exe)
  >  (name stubs_exe)
  >  (foreign_stubs
  >   (language c)
  >   (flags :standard -DNATIVE_CODE)
  >   (names c_stubs2))
  >  (foreign_stubs
  >   (mode native)
  >   (language c)
  >   (flags :standard -DNATIVE_CODE)
  >   (names c_stubs))
  >  (foreign_stubs
  >   (mode byte)
  >   (language c)
  >   (flags :standard)
  >   (names c_stubs)))
  > EOF

  $ cat >>stubs_exe.ml <<EOF
  > external stub_byte_or_native2 : unit -> int = "caml_b_or_n2"
  > let () = 
  >   Printf.printf "Byte (0) or native (1) 2 ? %i\n" (stub_byte_or_native2 ())
  > EOF

  $ dune exec ./stubs_exe.exe
  Byte (0) or native (1) ? 1
  Byte (0) or native (1) 2 ? 42

  $ dune exec ./stubs_exe.bc.exe
  Byte (0) or native (1) ? 0
  Byte (0) or native (1) 2 ? 42
