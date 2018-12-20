  $ dune build 2>&1 | grep -v ocamlc
  File "src/lib_rules.ml", line 137, characters 22-28: Assertion failed
  Backtrace:
  Raised at file "src/build_system.ml", line 1359, characters 9-14
  Called from file "src/fiber/fiber.ml", line 91, characters 8-15
  
  I must not segfault.  Uncertainty is the mind-killer.  Exceptions are
  the little-death that brings total obliteration.  I will fully express
  my cases.  Execution will pass over me and through me.  And when it
  has gone past, I will unwind the stack along its path.  Where the
  cases are handled there will be nothing.  Only I will remain.
