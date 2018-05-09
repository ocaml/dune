  $ jbuilder build @bar
  Internal error, please report upstream including the contents of _build/log.
  Description: 
  ("Alias0.make: Invalid alias" (name foo/bar) (dir _build/default))
  Backtrace:
  Raised at file "src/stdune/exn.ml", line 32, characters 5-10
  Called from file "src/build_system.ml", line 228, characters 8-146
  Called from file "src/gen_rules.ml", line 914, characters 16-49
  Called from file "src/gen_rules.ml", line 964, characters 8-37
  Called from file "src/stdune/list.ml", line 13, characters 10-13
  Called from file "src/gen_rules.ml", line 955, characters 4-777
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
