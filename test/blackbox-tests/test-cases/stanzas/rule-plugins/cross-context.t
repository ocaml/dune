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
  >  (libraries dune-rules-plugin))
  > (plugin
  >  (name generate)
  >  (executable ./rules.exe))
  > EOF
  $ cat > plugin/rules.ml <<'EOF'
  > open Dune_rules_plugin
  > let executable_context =
  >   Sys.executable_name |> Filename.dirname |> Filename.dirname |> Filename.basename
  > ;;
  > let () =
  >   run (fun _ ->
  >     [ Sexp.List
  >         [ Atom "rule"
  >         ; List [ Atom "target"; Atom "context" ]
  >         ; List
  >             [ Atom "action"
  >             ; List
  >                 [ Atom "with-stdout-to"
  >                 ; Atom "context"
  >                 ; List [ Atom "echo"; Atom executable_context ]
  >                 ]
  >             ]
  >         ]
  >     ])
  > EOF
  $ cat > use/dune <<'EOF'
  > (plugin.generate)
  > EOF

  $ dune build _build/cross/use/context
  $ cat _build/cross/use/context
  default
