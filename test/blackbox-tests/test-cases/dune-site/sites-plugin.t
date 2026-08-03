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

