If an action does not respect the dependency specification, it results in a broken
build. Dune fails to detect that:

  $ echo '(lang dune 1.11)' > dune-project
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

Some day, we should use the mtimes check from jenga to detect this.

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
  File "dune", line 1, characters 23-44:
  1 | (rule (target a) (deps (sandbox always none)) (action (bash "echo a > a")))
                             ^^^^^^^^^^^^^^^^^^^^^
  Error: Inconsistent sandboxing configuration. Sandboxing mode none is both
  allowed and disallowed
  Hint: dune files require fewer parentheses than jbuild files.
  If you just converted this file from a jbuild file, try removing these parentheses.
  [1]

When we don't pass [preserve_file_kind], the rules can observe the file kind changing based on sandbox mode chosen:

  $ rm -rf _build
  $ echo text-file > text-file
  $ true > dune
  $ echo '(rule (target t) (deps text-file) (action (bash "find text-file -printf '%y' > t")))' >> dune

  $ dune build t --sandbox symlink
  $ cat _build/default/t
  l

  $ dune build t --sandbox none
  $ cat _build/default/t
  f

When we pass [preserve_file_kind], the file type seen by the rule is preserved:

  $ true > dune
  $ echo '(rule (target t) (deps text-file (sandbox preserve_file_kind)) (action (bash "find text-file -printf '%y' > t")))' >> dune
  $ dune build t --sandbox symlink
  $ cat _build/default/t
  f
