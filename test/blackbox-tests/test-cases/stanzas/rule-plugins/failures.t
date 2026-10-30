Rule plugin failures, recursive expansion, and gated process lifecycle.
Callback exceptions and unexpected exits currently produce internal errors.

  $ cat > dune-project <<'EOF'
  > (lang dune 3.26)
  > (using plugin 0.1)
  > EOF
  $ mkdir plugin use
  $ cat > plugin/dune <<'EOF'
  > (executable
  >  (name rules)
  >  (libraries dune-rules-plugin unix))
  > (plugin
  >  (name generate)
  >  (executable ./rules.exe))
  > EOF
  $ cat > plugin/rules.ml <<'EOF'
  > open Dune_rules_plugin
  > let gate file = Filename.concat (Sys.getenv "PLUGIN_TEST_DIR") file
  > let signal file = close_out (open_out (gate file))
  > let rec wait_for file =
  >   if not (Sys.file_exists (gate file)) then (
  >     Unix.sleepf 0.01;
  >     wait_for file)
  > let rule target contents =
  >   [ Sexp.List
  >       [ Atom "rule"
  >       ; List [ Atom "target"; Atom target ]
  >       ; List
  >           [ Atom "action"
  >           ; List [ Atom "write-file"; Atom target; Quoted_string contents ]
  >           ]
  >       ]
  >   ]
  > let () =
  >   if Sys.file_exists (gate ".stall-start") then (
  >     Sys.remove (gate ".stall-start");
  >     signal ".starting";
  >     wait_for ".release-start");
  >   if Sys.getenv_opt "STALL_AT_EXIT" = Some "1" then
  >     Sys.set_signal Sys.sigterm Sys.Signal_ignore;
  >   at_exit (fun () ->
  >     if Sys.getenv_opt "STALL_AT_EXIT" = Some "1" then (
  >       signal ".exiting";
  >       wait_for ".release-exit"));
  >   run (function
  >     | [Sexp.Atom "error"] -> failwith "invalid plugin input"
  >     | [Sexp.Atom "crash"] -> exit 1
  >     | [Sexp.Atom "exit"] -> exit 0
  >     | [Sexp.Atom "block"; Atom target] ->
  >       signal ".blocked";
  >       wait_for ".release-request";
  >       rule target "stale response\n"
  >     | [Sexp.Atom "recurse"; Atom depth; Atom target] ->
  >       let depth = int_of_string depth in
  >       if depth = 0 then rule target "recursive expansion works\n"
  >       else
  >         [ Sexp.List
  >             [ Atom "plugin.generate"
  >             ; Atom "recurse"
  >             ; Atom (string_of_int (depth - 1))
  >             ; Atom target
  >             ]
  >         ]
  >     | _ -> [])
  > EOF
Keep synchronization gates under _build so they cannot trigger rebuilds.

  $ export PLUGIN_TEST_DIR="$PWD/_build/plugin-gates"
  $ mkdir -p "$PLUGIN_TEST_DIR"
  $ dune build plugin/rules.exe
  $ export DUNE_CONFIG__TRACE_RULE_PLUGINS=enabled
  $ export DUNE_TRACE=action,build
  $ trap '
  >   touch "$PLUGIN_TEST_DIR/.release-exit" "$PLUGIN_TEST_DIR/.release-request"
  >   touch "$PLUGIN_TEST_DIR/.release-start"
  > ' EXIT
  $ plugin_events () {
  >   trace_jq '
  >     [ .[]
  >     | select(.cat == "action" and (.name | startswith("rule-plugin-")))
  >     ]
  >     | group_by(.name)[]
  >     | "\(.[0].name) \(length)"'
  > }

Callback exceptions should be reported at the invocation. Currently they escape
as internal errors. Print only the exception record, without its backtrace.

  $ echo '(plugin.generate error)' > use/dune
  $ dune build use > .#error-output 2>&1
  [1]
  $ awk '/^Error: exception /,/^  }/' .#error-output
  Error: exception Response.E
    { payload = Some [ "Failure(\"invalid plugin input\")" ]
    ; message = "unexpected error"
    ; kind = Code_error
    }
  $ plugin_events
  rule-plugin-connected 1
  rule-plugin-disconnected 1
  rule-plugin-exited 1
  rule-plugin-request-finish 1
  rule-plugin-request-start 1
  rule-plugin-spawn 1
  rule-plugin-stop 1

Exiting while serving a request is also reported as an internal error.

  $ echo '(plugin.generate crash)' > use/dune
  $ dune build use > .#error-output 2>&1
  [1]
  $ awk '/^Error: exception /,/^  }/' .#error-output
  Error: exception Response.E
    { payload = Some [ [ "id"; "0" ] ]
    ; message =
        "Connection terminated. This request will never receive a response."
    ; kind = Connection_dead
    }
  $ plugin_events
  rule-plugin-connected 1
  rule-plugin-disconnected 1
  rule-plugin-exited 1
  rule-plugin-request-finish 1
  rule-plugin-request-start 1
  rule-plugin-spawn 1
  rule-plugin-stop 1

A plugin may generate further invocations of itself. The same process handles
all four requests, and the final expansion supplies an ordinary build rule.

  $ echo '(plugin.generate recurse 3 recursive.txt)' > use/dune
  $ dune build use/recursive.txt
  $ cat _build/default/use/recursive.txt
  recursive expansion works
  $ plugin_events
  rule-plugin-connected 1
  rule-plugin-disconnected 1
  rule-plugin-exited 1
  rule-plugin-request-finish 4
  rule-plugin-request-start 4
  rule-plugin-spawn 1
  rule-plugin-stop 1

Normal completion closes the connection and reaps the plugin, even if it
ignores SIGTERM and stalls in its exit handler. The scheduler's ordinary
SIGKILL escalation handles this without releasing the exit-handler gate.

  $ echo '(plugin.generate)' > use/dune
  $ STALL_AT_EXIT=1 dune build use > .#dune-output 2>&1
  $ cat .#dune-output
  $ test ! -e "$PLUGIN_TEST_DIR/.release-exit"
  $ plugin_events
  rule-plugin-connected 1
  rule-plugin-disconnected 1
  rule-plugin-exited 1
  rule-plugin-request-finish 1
  rule-plugin-request-start 1
  rule-plugin-spawn 1
  rule-plugin-stop 1

Dune reaps a connected plugin blocked in its exit handler when interrupted.
The exit-handler gate stays closed throughout shutdown.

  $ rm -f "$PLUGIN_TEST_DIR/.exiting"
  $ echo '(plugin.generate exit)' > use/dune
  $ STALL_AT_EXIT=1 dune build use > .#dune-output 2>&1 &
  $ DUNE_PID=$!
  $ wait_for_file "$PLUGIN_TEST_DIR/.exiting"
  $ wait_for_trace_jq_true 'any(.[]; .name == "rule-plugin-spawn")'
  $ PLUGIN_PID=$(trace_jq '
  >   first(.[] | select(.name == "rule-plugin-spawn")) | .args.pid')
  $ kill -INT "$DUNE_PID"
  $ wait "$DUNE_PID"
  [130]
  $ wait_for_pid_to_exit_with_timeout "$PLUGIN_PID" 200
  $ test ! -e "$PLUGIN_TEST_DIR/.release-exit"

Restarting a build terminates its blocked plugin and ignores the interrupted
RPC request. The replacement build succeeds without releasing the old callback.
The timeout is only a guard: on failure, release the callback to allow cleanup.

  $ echo '(plugin.generate block after-change.txt)' > use/dune
  $ start_dune
  $ dune rpc build --wait use/after-change.txt > .#build-result 2>&1 &
  $ REQUEST_PID=$!
  $ wait_for_file "$PLUGIN_TEST_DIR/.blocked"
  $ wait_for_trace_jq_true 'any(.[]; .name == "rule-plugin-spawn")'
  $ OLD_PLUGIN_PID=$(trace_jq '
  >   first(.[] | select(.name == "rule-plugin-spawn")) | .args.pid')
  $ echo '(plugin.generate recurse 0 after-change.txt)' > use/.#dune
  $ mv use/.#dune use/dune
  $ with_timeout dune rpc flush-file-watcher --wait
  $ wait_for_success_with_timeout .#build-result 1000 || {
  >   touch "$PLUGIN_TEST_DIR/.release-request"
  >   false
  > }
  $ wait "$REQUEST_PID"
  $ cat .#build-result
  Success
  $ awk '/^Error:/' .#dune-output
  $ cat _build/default/use/after-change.txt
  recursive expansion works
  $ wait_for_pid_to_exit_with_timeout "$OLD_PLUGIN_PID" 200
  $ test ! -e "$PLUGIN_TEST_DIR/.release-request"
  $ wait_for_trace_jq_true '
  >   [.[] | select(.name == "rule-plugin-spawn")] | length == 2'
  $ PLUGIN_PID=$(trace_jq '
  >   last(.[] | select(.name == "rule-plugin-spawn")) | .args.pid')
  $ stop_dune_quiet
  $ wait_for_pid_to_exit_with_timeout "$PLUGIN_PID" 200

The cancelled run's process was reaped before the replacement completed. The
watch server's final process is terminated by scheduler shutdown.

  $ plugin_events
  rule-plugin-connected 2
  rule-plugin-disconnected 1
  rule-plugin-exited 1
  rule-plugin-request-finish 2
  rule-plugin-request-start 2
  rule-plugin-spawn 2
  rule-plugin-stop 1

Cancellation also terminates a plugin that has not connected yet. The startup
marker is one-shot, so the replacement process can connect normally while the
original startup gate remains closed.

  $ touch "$PLUGIN_TEST_DIR/.stall-start"
  $ echo '(plugin.generate)' > use/dune
  $ start_dune
  $ dune rpc build --wait use/after-start.txt > .#build-result 2>&1 &
  $ REQUEST_PID=$!
  $ wait_for_file "$PLUGIN_TEST_DIR/.starting"
  $ wait_for_trace_jq_true 'any(.[]; .name == "rule-plugin-spawn")'
  $ OLD_PLUGIN_PID=$(trace_jq '
  >   first(.[] | select(.name == "rule-plugin-spawn")) | .args.pid')
  $ echo '(plugin.generate recurse 0 after-start.txt)' > use/.#dune
  $ mv use/.#dune use/dune
  $ with_timeout dune rpc flush-file-watcher --wait
  $ wait_for_success_with_timeout .#build-result 1000 || {
  >   touch "$PLUGIN_TEST_DIR/.release-start"
  >   false
  > }
  $ wait "$REQUEST_PID"
  $ cat .#build-result
  Success
  $ awk '/^Error:/' .#dune-output
  $ cat _build/default/use/after-start.txt
  recursive expansion works
  $ wait_for_pid_to_exit_with_timeout "$OLD_PLUGIN_PID" 200
  $ test ! -e "$PLUGIN_TEST_DIR/.release-start"
  $ wait_for_trace_jq_true '
  >   [.[] | select(.name == "rule-plugin-spawn")] | length == 2'
  $ PLUGIN_PID=$(trace_jq '
  >   last(.[] | select(.name == "rule-plugin-spawn")) | .args.pid')
  $ stop_dune_quiet
  $ wait_for_pid_to_exit_with_timeout "$PLUGIN_PID" 200
  $ plugin_events
  rule-plugin-connected 1
  rule-plugin-exited 1
  rule-plugin-request-finish 1
  rule-plugin-request-start 1
  rule-plugin-spawn 2
  rule-plugin-stop 1
