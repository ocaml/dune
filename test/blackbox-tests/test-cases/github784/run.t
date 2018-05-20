  $ dune build
  Error: exception Invalid_argument("Build_interpret.Rule.make: rule has no targets")
  Backtrace:
  Raised at file "pervasives.ml", line 33, characters 20-45
  Called from file "src/build_interpret.ml", line 209, characters 8-68
  Called from file "src/super_context.ml", line 373, characters 4-94
  Called from file "src/gen_rules.ml", line 259, characters 25-54
  Called from file "list.ml", line 82, characters 20-23
  Called from file "src/stdune/list.ml" (inlined), line 29, characters 29-39
  Called from file "src/gen_rules.ml", line 253, characters 12-960
  Called from file "src/stdune/hashtbl.ml", line 18, characters 12-17
  Called from file "src/gen_rules.ml", line 952, characters 16-39
  Called from file "src/gen_rules.ml", line 994, characters 19-30
  Called from file "src/build_system.ml", line 889, characters 6-62
  Called from file "src/build_system.ml", line 865, characters 6-59
  Re-raised at file "src/build_system.ml", line 876, characters 6-17
  Called from file "src/build_system.ml" (inlined), line 833, characters 32-63
  Called from file "src/build_system.ml", line 843, characters 4-24
  Called from file "src/build_interpret.ml", line 101, characters 24-40
  Called from file "src/build_interpret.ml", line 60, characters 31-43
  Called from file "src/build_interpret.ml", line 60, characters 31-43
  Called from file "src/build_system.ml", line 1215, characters 10-108
  Called from file "src/fiber/fiber.ml", line 359, characters 6-13
  [1]
