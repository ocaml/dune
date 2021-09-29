
  $ dune build ./app.exe @install
  $ dune exec ./app.exe
  Fatal error: exception Dune_site_plugins__Plugins.Thread_library_required_by_plugin_but_not_required_by_main_executable
  [2]

  $ sed -i -e "s/;TOREMOVE//" dune

  $ dune build ./app.exe @install
  $ dune exec ./app.exe
  Registration of Plugin1
  Main app starts...
  Plugin1 is doing something...
