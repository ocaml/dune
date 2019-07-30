If an action does not respect the dependency specification, it results in a broken
build. Dune fails to detect that:

  $ echo '(lang dune 1.12)' > dune-project
  $ true > dune
  $ echo '(rule (target a) (deps) (action (bash "echo a | tee a > b")))' >> dune
  $ echo '(rule (target b) (deps) (action (bash "echo b | tee a > b")))' >> dune
  $ echo '(rule (target c) (deps a b) (action (bash "cat a b > c")))' >> dune
  $ dune build c
  $ cat _build/default/c
  b
  b

  $ true > dune
  $ echo '(rule (target a) (deps) (action (bash "echo a > a")))' >> dune
  $ echo '(rule (target b) (deps) (action (bash "echo b > b")))' >> dune
  $ echo '(rule (target c) (deps a b) (action (bash "cat a b > c")))' >> dune
  $ dune build c
  $ cat _build/default/c
  b
  b

(it's not obvious what the correct result is on the first invocation, but the second
invocation is clearly broken (it uses a wrongly cached result))

These rules clearly depend on sandboxing. Specifying that makes the build
well-behaved:

  $ rm -rf _build
  $ true > dune
  $ echo '(rule (target a) (deps (sandbox always)) (action (bash "echo a | tee a > b")))' >> dune
  $ echo '(rule (target b) (deps (sandbox always)) (action (bash "echo b | tee a > b")))' >> dune
  $ echo '(rule (target c) (deps a b) (action (bash "cat a b > c")))' >> dune
  $ dune build c
  $ cat _build/default/c
  a
  b

Some errors:

  $ rm -rf _build
  $ true > dune
  $ echo '(rule (target a) (deps (sandbox always none)) (action (bash "echo a > a")))' >> dune
  $ dune build a
  File "dune", line 1, characters 32-43:
  1 | (rule (target a) (deps (sandbox always none)) (action (bash "echo a > a")))
                                      ^^^^^^^^^^^
  Error: Inconsistent sandboxing configuration. Sandboxing mode none is both
  allowed and disallowed
  [1]

When we don't pass [preserve_file_kind], the rules can observe the file kind changing based on sandbox mode chosen:

  $ rm -rf _build
  $ echo text-file > text-file
  $ true > dune
  $ echo '(rule (deps text-file) (target t) (action (with-stdout-to %{target} (run file -h text-file))))' >> dune

  $ dune build t --sandbox symlink
  $ cat _build/default/t | grep -Eo 'link|ASCII'
  link

  $ dune build t --sandbox none
  $ cat _build/default/t | grep -Eo 'link|ASCII'
  ASCII

When we pass [preserve_file_kind], the file type seen by the rule is preserved:

  $ true > dune
  $ echo '(rule (target t) (deps text-file (sandbox preserve_file_kind)) (action (with-stdout-to %{target} (run file -h text-file))))' >> dune
  $ dune build t --sandbox symlink
  $ cat _build/default/t | grep -Eo 'link|ASCII'
  ASCII

If rule fails to generate targets, we give a good error message, even with sandboxing:

  $ true > dune
  $ echo '(rule (target t) (deps (sandbox always)) (action (echo ":")))' >> dune
  $ dune build t
  File "src/dep.ml", line 68, characters 6-12: Assertion failed
  Backtrace:
  Raised at file "src/dep.ml", line 68, characters 6-53
  Called from file "list.ml", line 99, characters 22-25
  Called from file "src/stdune/list.ml" (inlined), line 5, characters 19-33
  Called from file "src/stdune/list.ml", line 39, characters 29-39
  Called from file "src/dep.ml", line 125, characters 6-74
  Called from file "src/build_system.ml", line 1426, characters 10-58
  Called from file "src/fiber/fiber.ml", line 112, characters 7-12
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
