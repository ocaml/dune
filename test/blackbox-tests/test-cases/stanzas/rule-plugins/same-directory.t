An executable whose rules depend on its own plugin invocation produces an
ordinary dependency cycle.

  $ cat > dune-project <<'EOF'
  > (lang dune 3.26)
  > (using plugin 0.1)
  > EOF
  $ cat > dune <<'EOF'
  > (executable
  >  (name rules)
  >  (libraries dune-rules-plugin-lwt lwt.unix))
  > (plugin
  >  (name generate)
  >  (executable ./rules.exe))
  > (plugin.generate)
  > EOF
  $ cat > rules.ml <<'EOF'
  > open Dune_rules_plugin_lwt
  > let () = Lwt_main.run (run (fun _ -> Lwt.return []))
  > EOF

  $ dune build
  Error: Dependency cycle between:
     rule plugin generate
  [1]

A definition beside its invocation can refer to an independently buildable
executable in a sibling directory.

  $ mkdir -p sibling/tool sibling/use
  $ cp dune-project sibling/dune-project
  $ cp rules.ml sibling/tool/rules.ml
  $ cat > sibling/tool/dune <<'EOF'
  > (executable
  >  (name rules)
  >  (libraries dune-rules-plugin-lwt lwt.unix))
  > EOF
  $ cat > sibling/use/dune <<'EOF'
  > (plugin
  >  (name generate)
  >  (executable ../tool/rules.exe))
  > (plugin.generate)
  > EOF
  $ cd sibling
  $ dune build tool/rules.exe
  $ dune build use
