Reproducing issue from https://github.com/ocaml/dune/issues/7597

  $ cat > dune-project << EOF
  > (lang dune 3.8)
  > EOF
  $ mkdir src
  $ cat > src/dune << EOF
  > (include_subdirs qualified)
  > (executable
  >  (name foo)
  >  (modules :standard \ bar))
  > EOF
  $ cat > src/foo.ml
  $ cat > src/bar.ml

  $ mkdir src/baz
  $ dune build @install
  File "src/dune_rules/module_trie.ml", line 106, characters 32-38:
  File "src/dune_rules/module_trie.ml", line 106, characters 32-38: Assertion
  failed
  Raised at Dune_rules__Module_trie.merge.base in file
    "src/dune_rules/module_trie.ml", line 106, characters 32-44
  Called from Dune_rules__Module_trie.merge.loop.(fun) in file
    "src/dune_rules/module_trie.ml", line 128, characters 48-61
  Called from Stdlib__Map.Make.merge in file "map.ml", line 398, characters
    44-63
  Called from Stdlib__Map.Make.merge in file "map.ml", line 398, characters
    64-79
  Called from Stdlib__Map.Make.merge in file "map.ml", line 398, characters
    25-40
  Called from Stdlib__List.fold_left in file "list.ml", line 121, characters
    24-34
  Called from Dune_rules__Ordered_set_lang.Eval.of_ast.loop in file
    "src/dune_rules/ordered_set_lang.ml", line 137, characters 19-28
  Called from Dune_rules__Modules_field_evaluator.eval.(fun) in file
    "src/dune_rules/modules_field_evaluator.ml", line 44, characters 18-62
  Called from Dune_rules__Modules_field_evaluator.eval0 in file
    "src/dune_rules/modules_field_evaluator.ml" (inlined), line 261, characters
    13-60
  Called from Dune_rules__Modules_field_evaluator.eval0 in file
    "src/dune_rules/modules_field_evaluator.ml", line 262, characters 16-56
  Called from Dune_rules__Modules_field_evaluator.eval in file
    "src/dune_rules/modules_field_evaluator.ml", line 332, characters 4-105
  Called from Dune_rules__Ml_sources.modules_of_stanzas.(fun) in file
    "src/dune_rules/ml_sources.ml", line 420, characters 10-216
  Called from Fiber__Scheduler.exec in file "otherlibs/fiber/src/scheduler.ml",
    line 73, characters 8-11
  Re-raised at Stdune__Exn.raise_with_backtrace in file
    "otherlibs/stdune/src/exn.ml", line 36, characters 27-56
  Called from Fiber__Scheduler.exec in file "otherlibs/fiber/src/scheduler.ml",
    line 73, characters 8-11
  Re-raised at Stdune__Exn.raise_with_backtrace in file
    "otherlibs/stdune/src/exn.ml", line 36, characters 27-56
  Called from Fiber__Scheduler.exec in file "otherlibs/fiber/src/scheduler.ml",
    line 73, characters 8-11
  Re-raised at Stdune__Exn.raise_with_backtrace in file
    "otherlibs/stdune/src/exn.ml", line 36, characters 27-56
  Called from Fiber__Scheduler.exec in file "otherlibs/fiber/src/scheduler.ml",
    line 73, characters 8-11
  Re-raised at Stdune__Exn.raise_with_backtrace in file
    "otherlibs/stdune/src/exn.ml", line 36, characters 27-56
  Called from Fiber__Scheduler.exec in file "otherlibs/fiber/src/scheduler.ml",
    line 73, characters 8-11
  Re-raised at Stdune__Exn.raise_with_backtrace in file
    "otherlibs/stdune/src/exn.ml", line 36, characters 27-56
  Called from Fiber__Scheduler.exec in file "otherlibs/fiber/src/scheduler.ml",
    line 73, characters 8-11
  
  I must not crash.  Uncertainty is the mind-killer. Exceptions are the
  little-death that brings total obliteration.  I will fully express my cases. 
  Execution will pass over me and through me.  And when it has gone past, I
  will unwind the stack along its path.  Where the cases are handled there will
  be nothing.  Only I will remain.
  [1]
