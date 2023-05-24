Demonstrate building a `include_subdirs qualified` project with `(modules
:standard \ exclusion)`

  $ dune build
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
  Called from Stdlib__List.fold_left in file "list.ml", line 121, characters
    24-34
  Called from Dune_lang__Ordered_set_lang.Eval.of_ast.loop in file
    "src/dune_lang/ordered_set_lang.ml", line 139, characters 19-28
  Called from Dune_rules__Modules_field_evaluator.eval.(fun) in file
    "src/dune_rules/modules_field_evaluator.ml", line 44, characters 18-62
  Called from Dune_rules__Modules_field_evaluator.eval0 in file
    "src/dune_rules/modules_field_evaluator.ml" (inlined), line 289, characters
    13-60
  Called from Dune_rules__Modules_field_evaluator.eval0 in file
    "src/dune_rules/modules_field_evaluator.ml", line 290, characters 16-56
  Called from Dune_rules__Modules_field_evaluator.eval in file
    "src/dune_rules/modules_field_evaluator.ml", line 360, characters 4-105
  Called from Dune_rules__Ml_sources.make_lib_modules in file
    "src/dune_rules/ml_sources.ml", line 342, characters 4-197
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

  $ find _build -iname "*.ml-gen" | sort | while read file; do echo "contents of $file"; cat $file; echo "--------"; done;
