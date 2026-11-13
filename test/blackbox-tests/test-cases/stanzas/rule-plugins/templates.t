Rule plugins preserve templates in generated stanzas.

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
  > let load_rules _ =
  >   Sexp.Parser.parse_string
  >     ~fname:"plugin"
  >     ~mode:Many
  >     "(rule (target out) (action (copy %{dep:input} out)))"
  >   |> List.map Sexp.Ast.remove_locs
  > let () = Lwt_main.run (run (Lwt.wrap1 load_rules))
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
  > open Dune_rules_plugin_lwt
  > let () =
  >   Lwt_main.run (run (Lwt.wrap1 (function
  >     | Sexp.Atom (A "atom")
  >       :: Quoted_string "quoted"
  >       :: Template
  >            { quoted = false
  >            ; parts = [ Pform { name = "profile"; payload = None; _ } ]
  >            ; _
  >            }
  >       :: Template
  >            { quoted = true
  >            ; parts =
  >                [ Text "prefix-"
  >                ; Pform { name = "dep"; payload = Some payload; _ }
  >                ]
  >            ; _
  >            }
  >       :: List [] :: stanzas
  >       when Sexp.Template.Pform.Payload.to_string payload = "input" -> stanzas
  >     | _ -> failwith "unexpected invocation arguments")))
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

Errors in generated template variables point to the invocation.

  $ cat > plugin/rules.ml <<'EOF'
  > open Dune_rules_plugin_lwt
  > let load_rules _ =
  >   Sexp.Parser.parse_string
  >     ~fname:"plugin"
  >     ~mode:Many
  >     "(rule (target bad) (action (echo %{unknown})))"
  >   |> List.map Sexp.Ast.remove_locs
  > let () = Lwt_main.run (run (Lwt.wrap1 load_rules))
  > EOF
  $ echo '(plugin.generate)' > use/dune
  $ dune build use/bad
  File "use/dune", line 1, characters 0-17:
  1 | (plugin.generate)
      ^^^^^^^^^^^^^^^^^
  Error: Unknown variable %{unknown}
  [1]
