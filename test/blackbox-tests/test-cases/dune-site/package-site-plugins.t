A package dependency should make a plugin available without building @install.

  $ unset DUNE_DIR_LOCATIONS
  $ write_sites_plugin_app_dune 3.20 '(package app)'
  $ write_sites_plugin_app_sources
  $ cat > app.ml <<'EOF'
  > let () =
  >   let module Plugins = Sites.Plugins.Plugins in
  >   Printf.printf "plugin site configured: %b\n%!" (Plugins.paths <> []);
  >   List.iter print_endline (Plugins.list ());
  >   Plugins.load_all ()
  > EOF
  $ mkdir plugin
  $ cat > plugin/dune-project <<'EOF'
  > (lang dune 3.20)
  > (using dune_site 0.1)
  > (package (name plugin1) (depends app))
  > EOF
  $ write_sites_plugin_dune
  $ write_sites_plugin_impl
  $ cat >> dune <<'EOF'
  > (rule
  >  (alias with-plugin)
  >  (deps (package app) (package plugin1))
  >  (action (run app)))
  > (rule
  >  (alias without-plugin)
  >  (deps (package app))
  >  (action (run app)))
  > EOF

  $ dune build @with-plugin
  plugin site configured: true
  $ dune build @without-plugin
  plugin site configured: true

A prebuilt install tree must not make the same plugin appear twice.

  $ cat >> dune <<'EOF'
  > (rule
  >  (target plugin-list)
  >  (deps (package app) (package plugin1))
  >  (action (with-stdout-to %{target} (run app))))
  > EOF
  $ dune build @install
  $ dune build plugin-list
  $ cat _build/default/plugin-list
  plugin site configured: true
  plugin1
  Registration of Plugin1
