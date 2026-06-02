Testsuite for (toplevel that loads plugins).

  $ cat > dune-project <<EOF
  > (lang dune 3.7)
  > (using dune_site 0.1)
  > (name top_with_plugins)
  > (wrapped_executables false)
  > (map_workspace_root false)
  > 
  > (package
  >  (name top_with_plugins)
  >  (sites (lib top_plugins)))
  > EOF

  $ cat > dune <<EOF
  > (executable
  >  (public_name top_with_plugins)
  >  (name top_with_plugins)
  >  (modes byte)
  >  (flags :standard -safe-string)
  >  (modules sites top_with_plugins)
  >  (link_flags (-linkall))
  >  (libraries compiler-libs.toplevel
  >   top_with_plugins.register dune-site dune-site.plugins 
  >   dune-site.toplevel))
  > 
  > (library
  >  (public_name top_with_plugins.register)
  >  (modes byte)
  >  (name registration)
  >  (modules registration))
  > 
  > (generate_sites_module
  >  (module sites)
  >  (plugins (top_with_plugins top_plugins)))
  > EOF

  $ write_toplevel_plugin_sources
  $ make_toplevel_plugin 1
  $ make_toplevel_plugin 2

  $ dune build @all 2>&1 | dune_cmd sanitize
  $ dune install --prefix _install --display short
  Installing _install/lib/top-plugin1/META
  Installing _install/lib/top-plugin1/dune-package
  Installing _install/lib/top-plugin1/opam
  Installing _install/lib/top-plugin1/plugin1_impl/plugin1_impl.cma
  Installing _install/lib/top-plugin1/plugin1_impl/plugin1_impl.cmi
  Installing _install/lib/top-plugin1/plugin1_impl/plugin1_impl.cmt
  Installing _install/lib/top-plugin1/plugin1_impl/plugin1_impl.ml
  Installing _install/lib/top_with_plugins/top_plugins/plugin1/META
  Installing _install/lib/top-plugin2/META
  Installing _install/lib/top-plugin2/dune-package
  Installing _install/lib/top-plugin2/opam
  Installing _install/lib/top-plugin2/plugin2_impl/plugin2_impl.cma
  Installing _install/lib/top-plugin2/plugin2_impl/plugin2_impl.cmi
  Installing _install/lib/top-plugin2/plugin2_impl/plugin2_impl.cmt
  Installing _install/lib/top-plugin2/plugin2_impl/plugin2_impl.ml
  Installing _install/lib/top_with_plugins/top_plugins/plugin2/META
  Installing _install/lib/top_with_plugins/META
  Installing _install/lib/top_with_plugins/dune-package
  Installing _install/lib/top_with_plugins/register/registration.cma
  Installing _install/lib/top_with_plugins/register/registration.cmi
  Installing _install/lib/top_with_plugins/register/registration.cmt
  Installing _install/lib/top_with_plugins/register/registration.ml
  Installing _install/bin/top_with_plugins
  $ export OCAMLPATH=$PWD/_install/lib
  $ ./_install/bin/top_with_plugins -noinit -no-version <<EOF
  > 2+2;;
  > #quit;;
  > EOF
  
  Main app really starts...
  Registration of Plugin1
  In register
  Done in register
  Done with registration of Plugin1
  Registration of Plugin2
  In register
  Done in register
  Done with registration of Plugin2
  Main app after loading plugins...
  Main app executing registered plugins...
  Plugin1 is doing something...
  Plugin2 is doing something...
  Main app after executing registered plugins...
  # - : int = 4
  # 

