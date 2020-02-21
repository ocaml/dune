  $ echo "(lang dune 2.3)" > dune-project
  $ cat >dune <<EOF
  > (executable (name foo) (modules foo foo-bar))
  > EOF
  $ dune exec ./foo.exe
  Error: exception { exn = ("Invalid Module_name.t", { s = "foo-bar" })
  ; backtrace =
      [ { ocaml =
            "Raised at file \"src/stdune/code_error.ml\", line 9, characters 30-62\n\
             Called from file \"src/dune/modules_field_evaluator.ml\", line 27, characters 15-38\n\
             Called from file \"src/dune/ordered_set_lang.ml\" (inlined), line 185, characters 33-41\n\
             Called from file \"src/dune/ordered_set_lang.ml\", line 188, characters 18-35\n\
             Called from file \"src/dune/ordered_set_lang.ml\", line 130, characters 16-28\n\
             Called from file \"list.ml\", line 103, characters 22-25\n\
             Called from file \"src/stdune/list.ml\", line 5, characters 19-33\n\
             Called from file \"src/dune/ordered_set_lang.ml\", line 133, characters 32-55\n\
             Called from file \"src/dune/modules_field_evaluator.ml\", line 37, characters 18-62\n\
             Called from file \"src/dune/modules_field_evaluator.ml\" (inlined), line 258, characters 13-58\n\
             Called from file \"src/dune/modules_field_evaluator.ml\", line 259, characters 16-55\n\
             Called from file \"src/dune/dir_contents.ml\", line 429, characters 12-194\n\
             Called from file \"src/stdune/list.ml\", line 67, characters 12-15\n\
             Called from file \"src/stdune/list.ml\" (inlined), line 72, characters 14-29\n\
             Called from file \"src/stdune/list.ml\", line 75, characters 13-42\n\
             Called from file \"src/stdune/exn_with_backtrace.ml\", line 9, characters 8-12\n\
             "
        ; memo = ("lazy-6", ())
        }
      ; { ocaml =
            "Raised at file \"src/memo/memo.ml\", line 574, characters 10-204\n\
             Called from file \"src/memo/memo.ml\" (inlined), line 874, characters 16-20\n\
             Called from file \"src/memo/memo.ml\", line 876, characters 37-46\n\
             Called from file \"src/stdune/exn_with_backtrace.ml\", line 9, characters 8-12\n\
             "
        ; memo = ("lazy-11", ())
        }
      ; { ocaml =
            "Raised at file \"src/memo/memo.ml\", line 580, characters 48-68\n\
             Called from file \"src/dune/dir_contents.ml\", line 172, characters 12-39\n\
             Called from file \"src/dune/exe_rules.ml\", line 15, characters 4-72\n\
             Called from file \"src/stdune/exn.ml\", line 13, characters 8-11\n\
             Re-raised at file \"src/stdune/exn.ml\", line 19, characters 4-11\n\
             Called from file \"src/memo/implicit_output.ml\", line 120, characters 4-162\n\
             Called from file \"src/dune/rules.ml\" (inlined), line 192, characters 20-71\n\
             Called from file \"src/dune/rules.ml\", line 195, characters 20-33\n\
             Called from file \"src/dune/build_system.ml\", line 1742, characters 19-34\n\
             Called from file \"src/dune/gen_rules.ml\", line 89, characters 8-70\n\
             Called from file \"src/dune/gen_rules.ml\", line 135, characters 6-96\n\
             Called from file \"list.ml\", line 121, characters 24-34\n\
             Called from file \"src/dune/gen_rules.ml\", line 138, characters 4-112\n\
             Called from file \"src/dune/gen_rules.ml\", line 230, characters 4-119\n\
             Called from file \"src/dune/gen_rules.ml\", line 358, characters 24-59\n\
             Called from file \"src/stdune/exn.ml\", line 13, characters 8-11\n\
             Re-raised at file \"src/stdune/exn.ml\", line 19, characters 4-11\n\
             Called from file \"src/memo/implicit_output.ml\", line 120, characters 4-162\n\
             Called from file \"src/dune/rules.ml\" (inlined), line 192, characters 20-71\n\
             Called from file \"src/dune/rules.ml\", line 195, characters 20-33\n\
             Called from file \"src/dune/build_system.ml\", line 845, characters 6-76\n\
             Called from file \"src/stdune/exn_with_backtrace.ml\", line 9, characters 8-12\n\
             "
        ; memo = ("load-dir", In_build_dir "default")
        }
      ]
  ; outer_call_stack = []
  }
  Backtrace:
  Raised at file "src/memo/memo.ml", line 580, characters 48-68
  Called from file "src/dune/build_system.ml", line 590, characters 10-23
  Called from file "src/stdune/exn_with_backtrace.ml", line 9, characters 8-12
  Re-raised at file "src/stdune/exn.ml", line 37, characters 27-56
  Called from file "src/dune/build_system.ml", line 1788, characters 34-74
  Called from file "bin/target.ml", line 75, characters 7-34
  Called from file "bin/target.ml", line 162, characters 12-35
  Called from file "list.ml", line 103, characters 22-25
  Called from file "src/stdune/list.ml", line 5, characters 19-33
  Called from file "bin/target.ml", line 157, characters 6-245
  Called from file "bin/exec.ml", line 62, characters 8-520
  Called from file "camlinternalLazy.ml", line 31, characters 17-27
  Re-raised at file "camlinternalLazy.ml", line 36, characters 4-11
  Called from file "vendor/cmdliner/src/cmdliner_term.ml", line 25, characters 19-24
  Called from file "vendor/cmdliner/src/cmdliner.ml", line 146, characters 9-16
  Called from file "vendor/cmdliner/src/cmdliner.ml", line 176, characters 18-36
  Called from file "vendor/cmdliner/src/cmdliner.ml", line 312, characters 20-46
  Called from file "bin/main.ml", line 240, characters 10-51
  
  I must not segfault.  Uncertainty is the mind-killer.  Exceptions are
  the little-death that brings total obliteration.  I will fully express
  my cases.  Execution will pass over me and through me.  And when it
  has gone past, I will unwind the stack along its path.  Where the
  cases are handled there will be nothing.  Only I will remain.
  [1]
