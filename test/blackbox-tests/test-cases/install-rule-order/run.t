Make sure that %{bin:..} is populated correctly. That means that install rules
must be evaluated before other rules that require substitutions are loaded.

  $ dune exec ./a/foo.exe
  Internal error, please report upstream including the contents of _build/log.
  Description:
  ("Build_system.get_collector called on closed directory"
   (dir (In_build_dir install/default/bin))
   (load_dir_stack ()))
  Backtrace:
  Raised at file "src/stdune/exn.ml", line 32, characters 5-10
  Called from file "src/build_system.ml", line 1589, characters 18-47
  Called from file "src/install_rules.ml", line 217, characters 6-77
  Called from file "list.ml", line 93, characters 22-25
  Called from file "src/stdune/list.ml", line 5, characters 19-33
  Called from file "src/install_rules.ml", line 321, characters 6-108
  Called from file "src/install_rules.ml", line 342, characters 6-22
  Called from file "map.ml", line 291, characters 20-25
  Called from file "src/gen_rules.ml", line 239, characters 4-25
  Called from file "map.ml", line 291, characters 20-25
  Called from file "src/gen_rules.ml", line 313, characters 2-60
  Called from file "src/fiber/fiber.ml", line 111, characters 22-27
  Called from file "src/fiber/fiber.ml", line 299, characters 6-13
  
  I must not segfault.  Uncertainty is the mind-killer.  Exceptions are
  the little-death that brings total obliteration.  I will fully express
  my cases.  Execution will pass over me and through me.  And when it
  has gone past, I will unwind the stack along its path.  Where the
  cases are handled there will be nothing.  Only I will remain.
  [1]
