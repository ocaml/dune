Test sites plugins (example from the manual)

  $ write_sites_plugin_app_dune 3.8

  $ write_sites_plugin_app_sources


  $ mkdir plugin
  $ cat > plugin/dune-project <<EOF
  > (lang dune 3.8)
  > (using dune_site 0.1)
  > 
  > (generate_opam_files true)
  > 
  > (package
  >  (name plugin1))
  > EOF

  $ write_sites_plugin_dune

  $ write_sites_plugin_impl

  $ dune build @install 2>&1 | dune_cmd sanitize
  $ dune exec ./app.exe
  Registration of Plugin1
  Main app starts...
  Plugin1 is doing something...

An empty library list is accepted. The plugin's META has no requirements,
so loading it does not run the library's initialization code.

  $ cat >plugin/dune <<EOF
  > (library
  >  (public_name plugin1.plugin1_impl)
  >  (name plugin1_impl)
  >  (modules plugin1_impl)
  >  (libraries app.register))
  > 
  > (plugin
  >  (name plugin1)
  >  (libraries)
  >  (site (app plugins)))
  > EOF

  $ dune build @install
  $ cat _build/default/plugin/.site/app/plugins/plugin1/META
  requires = ""

  $ dune exec ./app.exe
  Main app starts...
