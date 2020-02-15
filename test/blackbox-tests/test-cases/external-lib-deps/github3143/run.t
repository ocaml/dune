Reproduce #3143

  $ echo "(lang dune 2.3)" > dune-project
  $ touch dummypkg.opam
  $ cat >dune <<EOF
  > (library
  >  (public_name dummypkg)
  >  (libraries base doesnotexist.foo))
  > EOF
  $ dune external-lib-deps @install
  Internal error, please report upstream including the contents of _build/log.
  Description:
  ("Map.find_exn: failed to find key",
  { key = "doesnotexist.foo"; keys = [ "doesnotexist" ] })
  Backtrace:
  Raised at file "src/stdune/code_error.ml", line 9, characters 30-62
  Called from file "src/dune/findlib/findlib.ml", line 498, characters 8-44
  Called from file "src/dune/lib.ml", line 1843, characters 24-59
  Called from file "src/dune/lib.ml", line 1189, characters 10-25
  Called from file "src/dune/lib.ml", line 1203, characters 21-49
  Called from file "src/dune/lib.ml", line 1203, characters 21-49
  Called from file "src/dune/lib.ml", line 1181, characters 10-38
  Called from file "src/dune/lib.ml", line 1287, characters 25-76
  Called from file "list.ml", line 121, characters 24-34
  Called from file "src/dune/lib.ml", line 1269, characters 6-1023
  Called from file "src/dune/lib.ml" (inlined), line 1110, characters 9-75
  Called from file "src/dune/lib.ml", line 1109, characters 6-104
  Called from file "src/dune/lib.ml", line 1192, characters 12-42
  Called from file "src/dune/lib.ml", line 1851, characters 10-61
  Called from file "src/dune/super_context.ml", line 529, characters 18-56
  Called from file "src/dune/dir_with_dune.ml", line 23, characters 55-67
  Called from file "src/dune/super_context.ml", line 526, characters 6-692
  Called from file "src/dune/gen_rules.ml", line 412, characters 6-102
  Called from file "src/fiber/fiber.ml", line 102, characters 8-15
  
  I must not segfault.  Uncertainty is the mind-killer.  Exceptions are
  the little-death that brings total obliteration.  I will fully express
  my cases.  Execution will pass over me and through me.  And when it
  has gone past, I will unwind the stack along its path.  Where the
  cases are handled there will be nothing.  Only I will remain.
  [1]
