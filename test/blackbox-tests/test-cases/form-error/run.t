we're getting an acceptable error message when adding a macro form in an
inappropariate place:

  $ dune build
  Info: creating file dune-project with this contents: (lang dune 1.0)
  Internal error, please report upstream including the contents of _build/log.
  Description:
  ("expand_vars can't expand macros" (var "\%{read:x}"))
  Backtrace:
  Raised at file "src/stdune/exn.ml", line 32, characters 5-10
  Called from file "src/super_context.ml", line 105, characters 12-46
  Called from file "src/string_with_vars.ml", line 276, characters 12-32
  Called from file "src/string_with_vars.ml", line 252, characters 20-40
  Called from file "src/string_with_vars.ml", line 275, characters 4-487
  Called from file "src/super_context.ml", line 118, characters 4-38
  Called from file "src/gen_rules.ml", line 204, characters 21-68
  Called from file "src/gen_rules.ml", line 261, characters 25-68
  Called from file "list.ml", line 82, characters 20-23
  Called from file "src/stdune/list.ml" (inlined), line 29, characters 29-39
  Called from file "src/gen_rules.ml", line 254, characters 12-827
  Called from file "src/stdune/hashtbl.ml", line 80, characters 12-17
  Called from file "src/gen_rules.ml", line 1023, characters 16-39
  Called from file "src/gen_rules.ml", line 1086, characters 19-30
  Called from file "src/build_system.ml", line 917, characters 6-62
  Called from file "src/build_system.ml", line 893, characters 6-59
  Re-raised at file "src/build_system.ml", line 904, characters 6-17
  Called from file "src/build_system.ml" (inlined), line 861, characters 32-63
  Called from file "src/build_system.ml", line 871, characters 4-24
  Called from file "src/build_system.ml" (inlined), line 861, characters 32-63
  Called from file "src/build_system.ml", line 1115, characters 6-21
  Called from file "src/fiber/fiber.ml", line 160, characters 6-169
  [1]
