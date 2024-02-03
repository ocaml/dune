Showcase that using same library name in two workspaces is not possible at the moment

  $ mkdir -p a b
  $ cat > dune-project << EOF
  > (lang dune 3.13)
  > EOF

  $ cat > dune-workspace << EOF
  > (lang dune 3.13)
  > 
  > (context default)
  > 
  > (context
  >  (default
  >   (name alt-context)))
  > EOF
  $ cat > a/dune << EOF
  > (library
  >  (name foo)
  >  (enabled_if (= %{context_name} "default")))
  > EOF
  $ cat > a/foo.ml <<EOF
  > let x = "foo"
  > EOF

  $ cat > b/dune << EOF
  > (library
  >  (name foo)
  >  (enabled_if (= %{context_name} "alt-context")))
  > EOF
  $ cat > b/foo.ml <<EOF
  > let x = "foo"
  > EOF

  $ dune build
  Internal error, please report upstream including the contents of _build/log.
  Description:
    ("[gen_rules] returned rules in a directory that is not a descendant of the directory it was called for",
    { dir = In_build_dir "alt-context/a"
    ; example =
        Alias
          { dir = In_build_dir "alt-context/b/.foo.objs/byte"
          ; name = ".odoc-all"
          }
    })
  Raised at Stdune__Code_error.raise in file
    "otherlibs/stdune/src/code_error.ml", line 10, characters 30-62
  Called from
    Dune_engine__Load_rules.Load_rules.Normal.make_rules_gen_result.(fun) in
    file "src/dune_engine/load_rules.ml", line 549, characters 10-51
  Called from Fiber__Core.O.(>>|).(fun) in file "vendor/fiber/src/core.ml",
    line 253, characters 36-41
  Called from Fiber__Scheduler.exec in file "vendor/fiber/src/scheduler.ml",
    line 76, characters 8-11
  Re-raised at Stdune__Exn.raise_with_backtrace in file
    "otherlibs/stdune/src/exn.ml", line 38, characters 27-56
  Called from Fiber__Scheduler.exec in file "vendor/fiber/src/scheduler.ml",
    line 76, characters 8-11
  Re-raised at Stdune__Exn.raise_with_backtrace in file
    "otherlibs/stdune/src/exn.ml", line 38, characters 27-56
  Called from Fiber__Scheduler.exec in file "vendor/fiber/src/scheduler.ml",
    line 76, characters 8-11
  Re-raised at Stdune__Exn.raise_with_backtrace in file
    "otherlibs/stdune/src/exn.ml", line 38, characters 27-56
  Called from Fiber__Scheduler.exec in file "vendor/fiber/src/scheduler.ml",
    line 76, characters 8-11
  Re-raised at Stdune__Exn.raise_with_backtrace in file
    "otherlibs/stdune/src/exn.ml", line 38, characters 27-56
  Called from Fiber__Scheduler.exec in file "vendor/fiber/src/scheduler.ml",
    line 76, characters 8-11
  -> required by ("<unnamed>", ())
  -> required by ("load-dir", In_build_dir "alt-context/a")
  -> required by ("<unnamed>", ())
  -> required by
     ("build-alias", { dir = In_build_dir "alt-context"; name = "default" })
  -> required by ("toplevel", ())
  
  I must not crash.  Uncertainty is the mind-killer. Exceptions are the
  little-death that brings total obliteration.  I will fully express my cases. 
  Execution will pass over me and through me.  And when it has gone past, I
  will unwind the stack along its path.  Where the cases are handled there will
  be nothing.  Only I will remain.
  Internal error, please report upstream including the contents of _build/log.
  Description:
    ("[gen_rules] returned rules in a directory that is not a descendant of the directory it was called for",
    { dir = In_build_dir "default/b"
    ; example =
        Alias
          { dir = In_build_dir "default/a/.foo.objs/byte"; name = ".odoc-all" }
    })
  Raised at Stdune__Code_error.raise in file
    "otherlibs/stdune/src/code_error.ml", line 10, characters 30-62
  Called from
    Dune_engine__Load_rules.Load_rules.Normal.make_rules_gen_result.(fun) in
    file "src/dune_engine/load_rules.ml", line 549, characters 10-51
  Called from Fiber__Core.O.(>>|).(fun) in file "vendor/fiber/src/core.ml",
    line 253, characters 36-41
  Called from Fiber__Scheduler.exec in file "vendor/fiber/src/scheduler.ml",
    line 76, characters 8-11
  Re-raised at Stdune__Exn.raise_with_backtrace in file
    "otherlibs/stdune/src/exn.ml", line 38, characters 27-56
  Called from Fiber__Scheduler.exec in file "vendor/fiber/src/scheduler.ml",
    line 76, characters 8-11
  Re-raised at Stdune__Exn.raise_with_backtrace in file
    "otherlibs/stdune/src/exn.ml", line 38, characters 27-56
  Called from Fiber__Scheduler.exec in file "vendor/fiber/src/scheduler.ml",
    line 76, characters 8-11
  Re-raised at Stdune__Exn.raise_with_backtrace in file
    "otherlibs/stdune/src/exn.ml", line 38, characters 27-56
  Called from Fiber__Scheduler.exec in file "vendor/fiber/src/scheduler.ml",
    line 76, characters 8-11
  Re-raised at Stdune__Exn.raise_with_backtrace in file
    "otherlibs/stdune/src/exn.ml", line 38, characters 27-56
  Called from Fiber__Scheduler.exec in file "vendor/fiber/src/scheduler.ml",
    line 76, characters 8-11
  -> required by ("<unnamed>", ())
  -> required by ("load-dir", In_build_dir "default/b")
  -> required by ("<unnamed>", ())
  -> required by
     ("build-alias", { dir = In_build_dir "default"; name = "default" })
  -> required by ("toplevel", ())
  [1]
