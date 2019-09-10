  $ echo "(lang dune 1.11)" > dune-project
  $ cat >dune <<EOF
  > (dirs foo)
  > (data_only_dirs bar)
  > EOF
  $ mkdir bar && touch bar/x
  $ dune build
  Internal error, please report upstream including the contents of _build/log.
  Description:
  ("Sub_dirs.status: invalid combination",
  {t = {data_only = set {"bar"};
         vendored = set {};
         normal = set {}};
    dir = "bar"})
  Backtrace:
  Raised at file "src/stdune/code_error.ml", line 9, characters 30-62
  Called from file "src/dune/file_tree.ml", line 272, characters 22-54
  Called from file "list.ml", line 121, characters 24-34
  Called from file "src/dune/file_tree.ml", line 265, characters 11-1023
  Called from file "camlinternalLazy.ml", line 29, characters 17-27
  Re-raised at file "camlinternalLazy.ml", line 36, characters 4-11
  Called from file "src/dune/file_tree.ml" (inlined), line 93, characters 19-31
  Called from file "src/dune/file_tree.ml", line 118, characters 22-34
  Called from file "src/dune/dune_load.ml", line 212, characters 4-309
  Called from file "src/dune/main.ml", line 41, characters 13-44
  Called from file "bin/import.ml", line 55, characters 21-42
  Called from file "src/fiber/fiber.ml", line 114, characters 10-15
  
  I must not segfault.  Uncertainty is the mind-killer.  Exceptions are
  the little-death that brings total obliteration.  I will fully express
  my cases.  Execution will pass over me and through me.  And when it
  has gone past, I will unwind the stack along its path.  Where the
  cases are handled there will be nothing.  Only I will remain.
  [1]
