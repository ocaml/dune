A plugin cannot be invoked in the directory where it is defined because its
executable's rules have not been loaded yet.

  $ cat > dune-project <<'EOF'
  > (lang dune 3.26)
  > (using plugin 0.1)
  > EOF
  $ cat > dune <<'EOF'
  > (executable
  >  (name rules)
  >  (libraries dune-rules-plugin))
  > (plugin
  >  (name generate)
  >  (executable ./rules.exe))
  > (plugin.generate)
  > EOF
  $ cat > rules.ml <<'EOF'
  > open Dune_rules_plugin
  > let () = run (fun _ -> [])
  > EOF

  $ dune build
  File "dune", line 7, characters 0-17:
  7 | (plugin.generate)
      ^^^^^^^^^^^^^^^^^
  Error: Rule plugin "generate" cannot be used in the directory where it is
  defined.
  [1]
