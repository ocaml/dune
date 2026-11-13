A plugin executable may delegate the RPC connection to a worker process.
The worker inherits Dune's connection environment from the launcher.

  $ cat > dune-project <<'EOF'
  > (lang dune 3.26)
  > (using plugin 0.1)
  > EOF
  $ mkdir plugin use
  $ cat > plugin/dune <<'EOF'
  > (executable
  >  (name launcher)
  >  (modules launcher)
  >  (libraries unix)
  >  (link_deps worker.exe))
  > (executable
  >  (name worker)
  >  (modules worker)
  >  (libraries dune-rules-plugin-lwt lwt.unix unix))
  > (plugin
  >  (name generate)
  >  (executable ./launcher.exe))
  > EOF
  $ cat > plugin/launcher.ml <<'EOF'
  > let () =
  >   let worker =
  >     Filename.concat (Filename.dirname Sys.executable_name) "worker.exe"
  >   in
  >   let pid =
  >     Unix.create_process
  >       worker [| worker |] Unix.stdin Unix.stdout Unix.stderr
  >   in
  >   match snd (Unix.waitpid [] pid) with
  >   | Unix.WEXITED code -> exit code
  >   | Unix.WSIGNALED _ | Unix.WSTOPPED _ -> exit 1
  > EOF
  $ cat > plugin/worker.ml <<'EOF'
  > open Dune_rules_plugin_lwt
  > let () =
  >   Lwt_main.run (run (Lwt.wrap1 (fun _ ->
  >     let contents = Printf.sprintf "%d\n" (Unix.getpid ()) in
  >     [ Sexp.List
  >         [ Sexp.atom "rule"
  >         ; List [ Sexp.atom "target"; Sexp.atom "worker-pid" ]
  >         ; List
  >             [ Sexp.atom "action"
  >             ; List
  >                 [ Sexp.atom "write-file"; Sexp.atom "worker-pid"
  >                 ; Quoted_string contents
  >                 ]
  >             ]
  >         ]
  >     ])))
  > EOF
  $ echo '(plugin.generate)' > use/dune

Building the generated target also builds the worker through the launcher's
link dependency. The request is handled by a different process from the one
Dune spawned.

  $ DUNE_TRACE=action dune build use/worker-pid
  $ LAUNCHER_PID=$(trace_jq \
  >   '.[] | select(.name == "rule-plugin-spawn") | .args.pid')
  $ read WORKER_PID < _build/default/use/worker-pid
  $ test "$WORKER_PID" -ne "$LAUNCHER_PID" && echo 'worker handled the request'
  worker handled the request
