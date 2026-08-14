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
  > let signal file = close_out (open_out file)
  > let rec wait_for file =
  >   if not (Sys.file_exists file) then (
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
  >   at_exit (fun () ->
  >     if Sys.getenv_opt "STALL_AT_EXIT" = Some "1" then (
  >       signal ".exiting";
  >       wait_for ".release-exit"));
  >   run (function
  >     | [Sexp.Atom "error"] -> failwith "invalid plugin input"
  >     | [Sexp.Atom "crash"] -> exit 1
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
  $ dune build plugin/rules.exe
  $ export DUNE_CONFIG__TRACE_RULE_PLUGINS=enabled
  $ export DUNE_TRACE=action,build
  $ trap 'touch plugin/.release-exit plugin/.release-request' EXIT
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

Dune closes the connection after a successful build. Hold the plugin in its
exit handler until both sides have observed the disconnect, then release it.
The exit event is emitted only after the plugin process has been reaped.

  $ echo '(plugin.generate)' > use/dune
  $ STALL_AT_EXIT=1 dune build use > .#dune-output 2>&1 &
  $ DUNE_PID=$!
  $ wait_for_file plugin/.exiting
  $ wait_for_trace_jq_true 'any(.[]; .name == "rule-plugin-disconnected")'
  $ touch plugin/.release-exit
  $ wait "$DUNE_PID"
  $ cat .#dune-output
  $ plugin_events
  rule-plugin-connected 1
  rule-plugin-disconnected 1
  rule-plugin-exited 1
  rule-plugin-request-finish 1
  rule-plugin-request-start 1
  rule-plugin-spawn 1
  rule-plugin-stop 1

Change an invocation while its request is active. Wait for Dune to request a
restart before releasing the old response. The replacement build must use the
new invocation, not the stale response. No elapsed-time assertion is needed.

  $ echo '(plugin.generate block after-change.txt)' > use/dune
  $ start_dune
  $ dune rpc build --wait use/after-change.txt > .#build-result 2>&1 &
  $ REQUEST_PID=$!
  $ wait_for_file plugin/.blocked
  $ echo '(plugin.generate recurse 0 after-change.txt)' > use/dune
  $ with_timeout dune rpc flush-file-watcher --wait
  $ wait_for_trace_jq_true 'any(.[]; .name == "build-restart")'
  $ trace_jq '
  >   [ .[]
  >   | select(.name == "rule-plugin-request-start"
  >            or .name == "rule-plugin-request-finish"
  >            or .name == "build-restart")
  >   | .name
  >   ]'
  [
    "rule-plugin-request-start",
    "build-restart"
  ]
  $ touch plugin/.release-request
  $ wait "$REQUEST_PID"
  $ cat .#build-result
  Success
  $ cat _build/default/use/after-change.txt
  recursive expansion works
  $ PLUGIN_PID=$(trace_jq '
  >   last(.[] | select(.name == "rule-plugin-spawn")) | .args.pid')
  $ stop_dune_quiet
  $ wait_for_pid_to_exit_with_timeout "$PLUGIN_PID" 200

The previous run's process was retired normally. The watch server's final
process is terminated by scheduler shutdown rather than the plugin stop fiber.

  $ plugin_events
  rule-plugin-connected 2
  rule-plugin-disconnected 1
  rule-plugin-exited 1
  rule-plugin-request-finish 2
  rule-plugin-request-start 2
  rule-plugin-spawn 2
  rule-plugin-stop 1
