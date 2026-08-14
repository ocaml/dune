Rule plugins are host tools when generating rules for a target context.

  $ cat > dune-project <<'EOF'
  > (lang dune 3.26)
  > (using plugin 0.1)
  > EOF
  $ cat > dune-workspace <<'EOF'
  > (lang dune 3.26)
  > (context default)
  > (context
  >  (default
  >   (name cross)
  >   (host default)))
  > EOF
  $ mkdir plugin use
  $ cat > plugin/dune <<'EOF'
  > (executable
  >  (name rules)
  >  (libraries dune-rules-plugin-lwt lwt.unix))
  > (plugin
  >  (name generate)
  >  (executable ./rules.exe))
  > EOF
  $ cat > plugin/rules.ml <<'EOF'
  > open Dune_rules_plugin_lwt
  > let executable_context =
  >   Sys.executable_name |> Filename.dirname |> Filename.dirname |> Filename.basename
  > ;;
  > let () =
  >   Lwt_main.run (run (Lwt.wrap1 (fun _ ->
  >     [ Sexp.List
  >         [ Sexp.atom "rule"
  >         ; List [ Sexp.atom "target"; Sexp.atom "context" ]
  >         ; List
  >             [ Sexp.atom "action"
  >             ; List
  >                 [ Sexp.atom "with-stdout-to"
  >                 ; Sexp.atom "context"
  >                 ; List [ Sexp.atom "echo"; Sexp.atom executable_context ]
  >                 ]
  >             ]
  >         ]
  >     ])))
  > EOF
  $ cat > use/dune <<'EOF'
  > (plugin.generate)
  > EOF

  $ dune build _build/cross/use/context
  $ cat _build/cross/use/context
  default
