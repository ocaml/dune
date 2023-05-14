Demonstrate byte_complete executables not printing backtraces

  $ cat >dune-project <<EOF
  > (lang dune 3.8)
  > EOF
  $ cat >dune <<EOF
  > (executable
  >  (name foo)
  >  (modes byte_complete byte native))
  > EOF
  $ cat > foo.ml <<EOF
  > let () =
  >   Printexc.record_backtrace true;
  >   raise Not_found
  > EOF

Both native and bytecode modes print the backtrace

  $ dune exec ./foo.exe
  Fatal error: exception Not_found
  Raised at Dune__exe__Foo in file "foo.ml", line 3, characters 2-17
  [2]
  $ dune exec ./foo.bc
  Fatal error: exception Not_found
  Raised at Dune__exe__Foo in file "foo.ml", line 3, characters 2-17
  [2]

byte_complete does not print the backtrace

  $ dune exec ./foo.bc.exe
  Fatal error: exception Not_found
  [2]
