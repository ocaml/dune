A rule plugin may generate multiple stanzas while only being started once.

  $ cat > dune-project <<'EOF'
  > (lang dune 3.26)
  > (using plugin 0.1)
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
  > let atom = function
  >   | Sexp.Atom atom -> Sexp.Atom.to_string atom
  >   | Quoted_string _ | Template _ | List _ -> invalid_arg "atom expected"
  > ;;
  > let load_rules = function
  >   | [ argument ] ->
  >     let target = atom argument in
  >     [ Sexp.List
  >         [ Sexp.atom "rule"
  >         ; List [ Sexp.atom "target"; Sexp.atom target ]
  >         ; List
  >             [ Sexp.atom "action"
  >             ; List
  >                 [ Sexp.atom "with-stdout-to"
  >                 ; Sexp.atom target
  >                 ; List [ Sexp.atom "echo"; Sexp.atom target ]
  >                 ]
  >             ]
  >         ]
  >     ]
  >   | _ -> invalid_arg "one argument expected"
  > ;;
  > let () =
  >   prerr_endline "rules plugin started";
  >   Lwt_main.run (run (Lwt.wrap1 load_rules))
  > ;;
  > EOF

  $ cat > use/dune <<'EOF'
  > (plugin.generate first)
  > (plugin.generate second)
  > EOF

  $ dune build use/first use/second
  rules plugin started
  $ for target in first second; do cat "_build/default/use/$target"; echo; done
  first
  second

Plugin lifecycle events use the ordinary trace category filter.

  $ DUNE_TRACE=build dune build use/first use/second
  rules plugin started
  $ trace_jq '[.[] | select(.name | startswith("rule-plugin-"))] | length'
  0
