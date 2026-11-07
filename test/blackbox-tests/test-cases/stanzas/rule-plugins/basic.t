A rule plugin may generate multiple stanzas while only being started once.

  $ cat > dune-project <<'EOF'
  > (lang dune 3.26)
  > (using plugin 0.1)
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
  > let atom = function
  >   | Sexp.Atom atom -> atom
  >   | Quoted_string _ | Template _ | List _ -> invalid_arg "atom expected"
  > ;;
  > let load_rules = function
  >   | [ argument ] ->
  >     let target = atom argument in
  >     [ Sexp.List
  >         [ Atom "rule"
  >         ; List [ Atom "target"; Atom target ]
  >         ; List
  >             [ Atom "action"
  >             ; List
  >                 [ Atom "with-stdout-to"
  >                 ; Atom target
  >                 ; List [ Atom "echo"; Atom target ]
  >                 ]
  >             ]
  >         ]
  >     ]
  >   | _ -> invalid_arg "one argument expected"
  > ;;
  > let () =
  >   prerr_endline "rules plugin started";
  >   run load_rules
  > ;;
  > EOF

  $ cat > use/dune <<'EOF'
  > (plugin.generate first)
  > (plugin.generate second)
  > EOF

  $ DUNE_CONFIG__TRACE_RULE_PLUGINS=disabled dune build use/first use/second
  rules plugin started
  $ for target in first second; do cat "_build/default/use/$target"; echo; done
  first
  second

Plugin lifecycle tracing is disabled unless explicitly enabled.

  $ trace_jq '[.[] | select(.name | startswith("rule-plugin-"))] | length'
  0
