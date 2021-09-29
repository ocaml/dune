
  $ dune build ./app.exe @install
  $ dune exec ./app.exe
  The library is being used by two plugins finished initialization
  Fatal error: exception Dune_site_plugins__Plugins.Thread_library_required_by_plugin_but_not_required_by_main_executable
  [2]

  $ sed -i -e "s/;TOREMOVE//" dune

  $ dune build ./app.exe @install
  $ dune exec ./app.exe
  The library is being used by two plugins finished initialization
  Registration of Plugin1
  Registration of Plugin2
  Main app starts...
  Plugin1 is doing something...
  Plugin2 is doing something...
