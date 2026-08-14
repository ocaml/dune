Rule plugins have the same restrictions as dynamic includes.

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
  > let () =
  >   Lwt_main.run
  >     (run (fun _ ->
  >       Lwt.return
  >         [ Sexp.List
  >             [ Sexp.atom "library"; List [ Sexp.atom "name"; Sexp.atom "x" ] ] ]))
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

Atom construction uses the canonical Dune s-expression validation.

  $ cat > plugin/rules.ml <<'EOF'
  > open Dune_rules_plugin_lwt
  > let () =
  >   Lwt_main.run (run (fun _ -> Lwt.return [ Sexp.atom "invalid atom" ]))
  > EOF

  $ dune build use > .#error-output 2>&1
  [1]
  $ grep -qF 'Dune_lang.Atom.of_string got invalid atom' .#error-output \
  >   && echo 'canonical atom validation rejected invalid atom'
  canonical atom validation rejected invalid atom
