This is a bug occur that occurs when we're running dune in watch mode and
adding . to $CWD

Reproduce #6907

  $ . ./helpers.sh

  $ export PATH=.:$PATH
  $ echo "(lang dune 2.0)" > dune-project

  $ start_dune

  $ cat > x <<EOF
  > original-contents
  > EOF

  $ cat >dune <<EOF
  > (rule (with-stdout-to y (echo %{read:x})))
  > EOF

  $ build y
  Error: { payload = Some [ [ "id"; [ "auto"; "0" ] ] ]
  ; message =
      "connection terminated. this request will never receive a response"
  ; kind = Connection_dead
  }

  $ touch x

  $ build y
  Timed out
  Internal error, please report upstream including the contents of _build/log.
  Description:
    ("as_outside_build_dir_exn", { path = In_build_dir ".sync/0" })
  Raised at Stdune__Code_error.raise in file
    "otherlibs/stdune/src/code_error.ml", line 11, characters 30-62
  Called from Dune_engine__Fs_memo.handle_fs_event in file
    "src/dune_engine/fs_memo.ml", line 289, characters 13-47
  Called from
    Dune_engine__Scheduler.Run_once.handle_invalidation_events.handle_event in
    file "src/dune_engine/scheduler.ml", line 966, characters 26-55
  Called from Stdlib__List.fold_left in file "list.ml", line 121, characters
    24-34
  Called from Dune_engine__Scheduler.Run_once.build_input_change in file
    "src/dune_engine/scheduler.ml", line 1002, characters 23-56
  Called from Fiber.run.loop in file "otherlibs/fiber/src/fiber.ml", line 15,
    characters 51-60
  Called from Dune_engine__Scheduler.Run_once.run in file
    "src/dune_engine/scheduler.ml", line 1069, characters 10-50
  Re-raised at Stdune__Exn.raise_with_backtrace in file
    "otherlibs/stdune/src/exn.ml", line 36, characters 27-56
  Called from Cmdliner_term.app.(fun) in file
    "vendor/cmdliner/src/cmdliner_term.ml", line 24, characters 19-24
  Called from Cmdliner_eval.run_parser in file
    "vendor/cmdliner/src/cmdliner_eval.ml", line 34, characters 37-44
  Called from Cmdliner_eval.eval_value in file
    "vendor/cmdliner/src/cmdliner_eval.ml", line 202, characters 14-39
  Called from Dune__exe__Main in file "bin/main.ml", line 97, characters 10-41
  
  I must not crash.  Uncertainty is the mind-killer. Exceptions are the
  little-death that brings total obliteration.  I will fully express my cases. 
  Execution will pass over me and through me.  And when it has gone past, I
  will unwind the stack along its path.  Where the cases are handled there will
  be nothing.  Only I will remain.
  exit 1

  $ stop_dune
  Error: rpc server not running
  Internal error, please report upstream including the contents of _build/log.
  Description:
    ("as_outside_build_dir_exn", { path = In_build_dir ".sync/0" })
  Raised at Stdune__Code_error.raise in file
    "otherlibs/stdune/src/code_error.ml", line 11, characters 30-62
  Called from Dune_engine__Fs_memo.handle_fs_event in file
    "src/dune_engine/fs_memo.ml", line 289, characters 13-47
  Called from
    Dune_engine__Scheduler.Run_once.handle_invalidation_events.handle_event in
    file "src/dune_engine/scheduler.ml", line 966, characters 26-55
  Called from Stdlib__List.fold_left in file "list.ml", line 121, characters
    24-34
  Called from Dune_engine__Scheduler.Run_once.build_input_change in file
    "src/dune_engine/scheduler.ml", line 1002, characters 23-56
  Called from Fiber.run.loop in file "otherlibs/fiber/src/fiber.ml", line 15,
    characters 51-60
  Called from Dune_engine__Scheduler.Run_once.run in file
    "src/dune_engine/scheduler.ml", line 1069, characters 10-50
  Re-raised at Stdune__Exn.raise_with_backtrace in file
    "otherlibs/stdune/src/exn.ml", line 36, characters 27-56
  Called from Cmdliner_term.app.(fun) in file
    "vendor/cmdliner/src/cmdliner_term.ml", line 24, characters 19-24
  Called from Cmdliner_eval.run_parser in file
    "vendor/cmdliner/src/cmdliner_eval.ml", line 34, characters 37-44
  Called from Cmdliner_eval.eval_value in file
    "vendor/cmdliner/src/cmdliner_eval.ml", line 202, characters 14-39
  Called from Dune__exe__Main in file "bin/main.ml", line 97, characters 10-41
  
  I must not crash.  Uncertainty is the mind-killer. Exceptions are the
  little-death that brings total obliteration.  I will fully express my cases. 
  Execution will pass over me and through me.  And when it has gone past, I
  will unwind the stack along its path.  Where the cases are handled there will
  be nothing.  Only I will remain.
  exit 1
