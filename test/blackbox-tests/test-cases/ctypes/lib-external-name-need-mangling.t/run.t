Bind to a C library which has a name invalid in ocaml

  $ LIBEX=$(realpath "$PWD/../libneed-mangling")

This silly looking hack is to make sure the .pc file points to the sandbox. We
cannot set ${prefix} to be interpreted relative to the .pc itself ufortunately
  $ awk "BEGIN{print \"prefix=$LIBEX\"} {print}" $LIBEX/need-mangling.pc > need-mangling.pc

  $ DYLD_LIBRARY_PATH="$LIBEX" LD_LIBRARY_PATH="$LIBEX" PKG_CONFIG_PATH="$PWD:$PKG_CONFIG_PATH" dune exec ./example.exe
  Internal error, please report upstream including the contents of _build/log.
  Description:
    ("Invalid Module_name.t", { s = "need-mangling__c_generated_types" })
  Raised at Stdune__Code_error.raise in file "otherlibs/stdune/code_error.ml",
    line 11, characters 30-62
  Called from Dune_rules__Ctypes_rules.Stanza_util.generated_modules in file
    "src/dune_rules/ctypes_rules.ml", line 129, characters 8-39
  Called from Dune_rules__Ctypes_rules.Stanza_util.generated_ml_and_c_files in
    file "src/dune_rules/ctypes_rules.ml", line 142, characters 6-30
  Called from Dune_rules__Dir_contents.Load.load_text_files.(fun) in file
    "src/dune_rules/dir_contents.ml", line 180, characters 31-75
  Called from Fiber__Scheduler.exec in file "src/fiber/scheduler.ml", line 69,
    characters 8-11
  Re-raised at Stdune__Exn.raise_with_backtrace in file
    "otherlibs/stdune/exn.ml", line 36, characters 27-56
  Called from Fiber__Scheduler.exec in file "src/fiber/scheduler.ml", line 69,
    characters 8-11
  Re-raised at Stdune__Exn.raise_with_backtrace in file
    "otherlibs/stdune/exn.ml", line 36, characters 27-56
  Called from Fiber__Scheduler.exec in file "src/fiber/scheduler.ml", line 69,
    characters 8-11
  Re-raised at Stdune__Exn.raise_with_backtrace in file
    "otherlibs/stdune/exn.ml", line 36, characters 27-56
  Called from Fiber__Scheduler.exec in file "src/fiber/scheduler.ml", line 69,
    characters 8-11
  Re-raised at Stdune__Exn.raise_with_backtrace in file
    "otherlibs/stdune/exn.ml", line 36, characters 27-56
  Called from Fiber__Scheduler.exec in file "src/fiber/scheduler.ml", line 69,
    characters 8-11
  Re-raised at Stdune__Exn.raise_with_backtrace in file
    "otherlibs/stdune/exn.ml", line 36, characters 27-56
  Called from Fiber__Scheduler.exec in file "src/fiber/scheduler.ml", line 69,
    characters 8-11
  Re-raised at Stdune__Exn.raise_with_backtrace in file
    "otherlibs/stdune/exn.ml", line 36, characters 27-56
  Called from Fiber__Scheduler.exec in file "src/fiber/scheduler.ml", line 69,
    characters 8-11
  Re-raised at Stdune__Exn.raise_with_backtrace in file
    "otherlibs/stdune/exn.ml", line 36, characters 27-56
  Called from Fiber__Scheduler.exec in file "src/fiber/scheduler.ml", line 69,
    characters 8-11
  Re-raised at Stdune__Exn.raise_with_backtrace in file
    "otherlibs/stdune/exn.ml", line 36, characters 27-56
  Called from Fiber__Scheduler.exec in file "src/fiber/scheduler.ml", line 69,
    characters 8-11
  Re-raised at Stdune__Exn.raise_with_backtrace in file
    "otherlibs/stdune/exn.ml", line 36, characters 27-56
  Called from Fiber__Scheduler.exec in file "src/fiber/scheduler.ml", line 69,
    characters 8-11
  Re-raised at Stdune__Exn.raise_with_backtrace in file
    "otherlibs/stdune/exn.ml", line 36, characters 27-56
  Called from Fiber__Scheduler.exec in file "src/fiber/scheduler.ml", line 69,
    characters 8-11
  Re-raised at Stdune__Exn.raise_with_backtrace in file
    "otherlibs/stdune/exn.ml", line 36, characters 27-56
  Called from Fiber__Scheduler.exec in file "src/fiber/scheduler.ml", line 69,
    characters 8-11
  Re-raised at Stdune__Exn.raise_with_backtrace in file
    "otherlibs/stdune/exn.ml", line 36, characters 27-56
  Called from Fiber__Scheduler.exec in file "src/fiber/scheduler.ml", line 69,
    characters 8-11
  -> required by ("<unnamed>", ())
  -> required by ("<unnamed>", ())
  -> required by ("<unnamed>", ())
  -> required by ("Rule.make", ())
  -> required by ("execute-rule", { id = 7; info = Internal })
  -> required by ("<unnamed>", ())
  -> required by
     ("build-file", In_build_dir "default/.merlin-conf/exe-example")
  -> required by ("Rule.set_action", ())
  -> required by
     ("execute-rule",
     { id = 6
     ; info =
         From_dune_file
           { pos_fname = "dune"
           ; start = { pos_lnum = 2; pos_bol = 12; pos_cnum = 20 }
           ; stop = { pos_lnum = 2; pos_bol = 12; pos_cnum = 27 }
           }
     })
  -> required by ("<unnamed>", ())
  -> required by ("build-file", In_build_dir "default/example.exe")
  
  I must not crash.  Uncertainty is the mind-killer. Exceptions are the
  little-death that brings total obliteration.  I will fully express my cases. 
  Execution will pass over me and through me.  And when it has gone past, I
  will unwind the stack along its path.  Where the cases are handled there will
  be nothing.  Only I will remain.
  [1]
