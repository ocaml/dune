Reproduction case for #1560: by default, `dune` files inside the .git
directory should be ignored

  $ echo '(lang dune 1.11)' > dune-project
  $ true > dune
  $ echo '(rule (target a) (deps) (action (bash "echo a > a; echo a > b")))' >> dune
  $ echo '(rule (target b) (deps) (action (bash "echo a > a; echo a > b")))' >> dune
  $ echo '(rule (target c) (deps a b) (action (bash "cat a b > c")))' >> dune
  $ dune build c
  $ cat _build/default/c
  a
  a

If an action does not respect the dependency specification, it results in a broken
build. Dune fails to detect that.

Some day, we should use the mtimes check from jenga to detect it.

  $ rm -rf _build
  $ true > dune
  $ echo '(rule (target a) (deps (sandbox always)) (action (bash "echo a > a; echo a > b")))' >> dune
  $ echo '(rule (target b) (deps (sandbox always)) (action (bash "echo a > a; echo a > b")))' >> dune
  $ echo '(rule (target c) (deps a b) (action (bash "cat a b > c")))' >> dune
  $ dune build c
  Internal error, please report upstream including the contents of _build/log.
  Description:
  ("This rule forbids all sandboxing modes (but it also requires sandboxing)",
  {})
  Backtrace:
  Raised at file "src/stdune/code_error.ml", line 9, characters 2-29
  Called from file "src/build_system.ml", line 1459, characters 6-118
  Called from file "src/fiber/fiber.ml", line 112, characters 7-12
  Re-raised at file "src/stdune/exn.ml", line 39, characters 38-65
  Called from file "src/fiber/fiber.ml", line 82, characters 8-15
  Re-raised at file "src/stdune/exn.ml", line 39, characters 38-65
  Called from file "src/fiber/fiber.ml", line 82, characters 8-15
  Re-raised at file "src/stdune/exn.ml", line 39, characters 38-65
  Called from file "src/fiber/fiber.ml", line 82, characters 8-15
  
  I must not segfault.  Uncertainty is the mind-killer.  Exceptions are
  the little-death that brings total obliteration.  I will fully express
  my cases.  Execution will pass over me and through me.  And when it
  has gone past, I will unwind the stack along its path.  Where the
  cases are handled there will be nothing.  Only I will remain.
  [1]
  $ cat _build/default/c
  cat: _build/default/c: No such file or directory
  [1]

# CR aalekseyev: fix this!
