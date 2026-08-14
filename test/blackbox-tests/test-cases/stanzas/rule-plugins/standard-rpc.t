Rule plugins should use DUNE_RPC to connect to the main RPC server, including
when their working directory differs from the workspace root.

  $ cat > dune-project <<'EOF'
  > (lang dune 3.26)
  > (using plugin 0.1)
  > EOF
  $ mkdir plugin use
  $ cat > plugin/dune <<'EOF'
  > (executable
  >  (name rules)
  >  (libraries dune-rules-plugin-lwt lwt.unix unix))
  > (plugin
  >  (name generate)
  >  (executable ./rules.exe))
  > EOF
  $ cat > plugin/rules.ml <<'EOF'
  > open Dune_rules_plugin_lwt
  > let fail message =
  >   prerr_endline ("plugin: " ^ message);
  >   exit 1
  > let () =
  >   (match Sys.getenv_opt "DUNE_RPC" with
  >    | None -> fail "missing DUNE_RPC"
  >    | Some _ -> ());
  >   (match Sys.getenv_opt "DUNE_RULES_PLUGIN_HOST",
  >          Sys.getenv_opt "DUNE_RULES_PLUGIN_PORT" with
  >    | None, None -> ()
  >    | _ -> fail "legacy RPC endpoint");
  >   (match Unix.system "dune rpc ping" with
  >    | Unix.WEXITED 0 -> ()
  >    | _ -> fail "main RPC ping failed");
  >   prerr_endline "plugin: main RPC ping succeeded";
  >   Lwt_main.run
  >     (let open Lwt.Syntax in
  >      let* () = Lwt.pause () in
  >      run (fun _ ->
  >        let* () = Lwt.pause () in
  >        Lwt.return
  >          [ Sexp.List
  >              [ Sexp.atom "rule"
  >              ; List [ Sexp.atom "target"; Sexp.atom "result" ]
  >              ; List
  >                  [ Sexp.atom "action"
  >                  ; List
  >                      [ Sexp.atom "write-file"; Sexp.atom "result"
  >                      ; Quoted_string "shared RPC server\n"
  >                      ]
  >                  ]
  >              ]
  >          ]))
  > EOF
  $ echo '(plugin.generate)' > use/dune

The plugin can call a public RPC method before serving rule-generation requests
on the same server. It serves requests from an existing Lwt loop, and its
callback can yield before returning rules.

  $ with_timeout dune build use/result > .#dune-output 2>&1
  $ grep '^plugin:' .#dune-output
  plugin: main RPC ping succeeded
  $ cat _build/default/use/result
  shared RPC server
