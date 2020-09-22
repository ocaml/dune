Creating an invalid (deprecated_libary_name ..) is an error

  $ cat >dune-project <<EOF
  > (lang dune 2.0)
  > (package (name a))
  > EOF

  $ cat >dune <<EOF
  > (deprecated_library_name (old_public_name a) (new_public_name a))
  > EOF

  $ dune build @all
  File "dune", line 1, characters 62-63:
  1 | (deprecated_library_name (old_public_name a) (new_public_name a))
                                                                    ^
  Internal error, please report upstream including the contents of _build/log.
  Description:
    ("Invalid redirect", { to_ = "a" })
  Raised at file "src/stdune/code_error.ml", line 9, characters 30-62
  Called from file "src/dune_rules/scope.ml", line 66, characters 14-74
  Called from file "list.ml", line 103, characters 22-25
  Called from file "src/stdune/list.ml" (inlined), line 5, characters 19-33
  Called from file "src/stdune/list.ml", line 40, characters 29-39
  Called from file "src/dune_rules/scope.ml", line 57, characters 6-1023
  Called from file "src/dune_rules/scope.ml", line 235, characters 10-76
  Called from file "map.ml", line 393, characters 44-63
  Called from file "src/dune_rules/scope.ml", line 250, characters 6-70
  Called from file "src/dune_rules/super_context.ml", line 459, characters 4-75
  Called from file "src/dune_rules/gen_rules.ml", line 425, characters 6-70
  Called from file "src/fiber/fiber.ml", line 127, characters 18-21
  
  I must not crash.  Uncertainty is the mind-killer. Exceptions are the
  little-death that brings total obliteration.  I will fully express my cases. 
  Execution will pass over me and through me.  And when it has gone past, I
  will unwind the stack along its path.  Where the cases are handled there will
  be nothing.  Only I will remain.
  [1]
