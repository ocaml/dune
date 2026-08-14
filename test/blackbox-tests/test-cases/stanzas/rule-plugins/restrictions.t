Rule plugins have the same restrictions as dynamic includes.

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
  > let () = run (fun _ -> [ Sexp.List [ Atom "library"; List [ Atom "name"; Atom "x" ] ] ])
  > EOF
  $ cat > use/dune <<'EOF'
  > (plugin.generate)
  > EOF

  $ dune build use
  File "use/dune", line 1, characters 0-17:
  1 | (plugin.generate)
      ^^^^^^^^^^^^^^^^^
  Error: This stanza cannot be generated dynamically
  [1]

Invalid atoms are rejected at the plugin invocation.

  $ cat > plugin/rules.ml <<'EOF'
  > open Dune_rules_plugin
  > let () = run (fun _ -> [ Sexp.Atom "invalid atom" ])
  > EOF

  $ dune build use
  File "use/dune", line 1, characters 0-17:
  1 | (plugin.generate)
      ^^^^^^^^^^^^^^^^^
  Error: Rule plugin "generate" returned an invalid s-expression:
  (4:atom12:invalid atom)
  [1]
