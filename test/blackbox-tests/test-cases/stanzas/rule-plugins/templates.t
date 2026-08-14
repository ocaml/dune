Rule plugins preserve templates in generated stanzas.

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
  > let () =
  >   run (fun _ ->
  >     [ Sexp.List
  >         [ Atom "rule"
  >         ; List [ Atom "target"; Atom "out" ]
  >         ; List
  >             [ Atom "action"
  >             ; List
  >                 [ Atom "copy"
  >                 ; Template
  >                     { quoted = false
  >                     ; parts = [ Pform { name = "dep"; payload = Some "input" } ]
  >                     }
  >                 ; Atom "out"
  >                 ]
  >             ]
  >         ]
  >     ])
  > EOF
  $ cat > use/dune <<'EOF'
  > (plugin.generate)
  > EOF
  $ echo contents > use/input

  $ dune build use/out
  $ cat _build/default/use/out
  contents

Round-tripping invocation arguments preserves atoms, quoted strings, empty
lists, and quoted and unquoted templates with and without payloads.

  $ cat > plugin/rules.ml <<'EOF'
  > open Dune_rules_plugin
  > let () =
  >   run (function
  >     | Sexp.Atom "atom"
  >       :: Quoted_string "quoted"
  >       :: Template
  >            { quoted = false
  >            ; parts = [ Pform { name = "profile"; payload = None } ]
  >            }
  >       :: Template
  >            { quoted = true
  >            ; parts =
  >                [ Text "prefix-"
  >                ; Pform { name = "dep"; payload = Some "input" }
  >                ]
  >            }
  >       :: List [] :: stanzas -> stanzas
  >     | _ -> failwith "unexpected invocation arguments")
  > EOF
  $ cat > use/dune <<'EOF'
  > (plugin.generate
  >  atom "quoted" %{profile} "prefix-%{dep:input}" ()
  >  (rule
  >   (target roundtrip)
  >   (action
  >    (with-stdout-to %{target}
  >     (echo bare "quoted text" "" "profile-%{profile}" "%{dep:input}"
  >      %{read-lines:input})))))
  > EOF

  $ dune build use/roundtrip
  $ cat _build/default/use/roundtrip
  bare quoted text  profile-dev ./input contents
