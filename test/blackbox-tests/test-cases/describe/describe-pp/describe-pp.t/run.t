We can show the preprocessed output of a source code

  $ dune describe pp src/main.ml
  ;;Util.log "Hello, world!"

Re-running the command keeps showing output

  $ dune describe pp src/main.ml
  ;;Util.log "Hello, world!"

We can also show the original source if it is not preprocessed

  $ dune describe pp src/util.ml
  let log str = print_endline str

We also make sure that the dump file is not present

  $ dune_cmd exists profile.dump
  false

This also works for reason code

  $ dune describe pp src/main_re.re
  # 1 "src/main_re.pp.re.ml"
  # 1 "src/main_re.pp.re"
  Util.log ("Hello, world!")

  $ dune describe pp lib/subdir/bazy.ml
  Internal error, please report upstream including the contents of _build/log.
  Description:
    ("Lib.DB.get_compile_info got library that doesn't exist",
     { lib_id =
         Local
           { name = "foo"
           ; loc = "lib/dune:4"
           ; src_dir = In_source_tree "lib/subdir"
           }
     })
  Raised at Stdune__Code_error.raise in file
    "otherlibs/stdune/src/code_error.ml", line 10, characters 30-62
  Called from Fiber__Core.O.(>>|).(fun) in file "vendor/fiber/src/core.ml",
    line 259, characters 36-41
  Called from Fiber__Scheduler.exec in file "vendor/fiber/src/scheduler.ml",
    line 79, characters 8-11
  Re-raised at Stdune__Exn.raise_with_backtrace in file
    "otherlibs/stdune/src/exn.ml", line 38, characters 27-56
  Called from Fiber__Scheduler.exec in file "vendor/fiber/src/scheduler.ml",
    line 79, characters 8-11
  
  I must not crash.  Uncertainty is the mind-killer. Exceptions are the
  little-death that brings total obliteration.  I will fully express my cases. 
  Execution will pass over me and through me.  And when it has gone past, I
  will unwind the stack along its path.  Where the cases are handled there will
  be nothing.  Only I will remain.
  [1]
