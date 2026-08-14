Plugin names are resolved in the project containing the invocation.

  $ cat > dune-project <<'EOF'
  > (lang dune 3.26)
  > (using plugin 0.1)
  > EOF
  $ mkdir outer-plugin outer-use nested
  $ cat > outer-plugin/dune <<'EOF'
  > (executable
  >  (name rules)
  >  (libraries dune-rules-plugin))
  > (plugin
  >  (name generate)
  >  (executable ./rules.exe))
  > EOF
  $ cat > outer-plugin/rules.ml <<'EOF'
  > open Dune_rules_plugin
  > let () =
  >   run (fun _ ->
  >     [ Sexp.List
  >         [ Atom "rule"
  >         ; List [ Atom "target"; Atom "result" ]
  >         ; List
  >             [ Atom "action"
  >             ; List
  >                 [ Atom "with-stdout-to"
  >                 ; Atom "result"
  >                 ; List [ Atom "echo"; Atom "outer" ]
  >                 ]
  >             ]
  >         ]
  >     ])
  > EOF
  $ cat > outer-use/dune <<'EOF'
  > (plugin.generate)
  > EOF

  $ cat > nested/dune-project <<'EOF'
  > (lang dune 3.26)
  > (using plugin 0.1)
  > EOF
  $ mkdir nested/plugin nested/use
  $ cat > nested/plugin/dune <<'EOF'
  > (executable
  >  (name rules)
  >  (libraries dune-rules-plugin))
  > (plugin
  >  (name generate)
  >  (executable ./rules.exe))
  > EOF
  $ sed 's/outer/nested/' outer-plugin/rules.ml > nested/plugin/rules.ml
  $ cat > nested/use/dune <<'EOF'
  > (plugin.generate)
  > EOF

  $ dune build outer-use/result nested/use/result
  $ for result in outer-use/result nested/use/result; do
  >   cat "_build/default/$result"
  >   echo
  > done
  outer
  nested
