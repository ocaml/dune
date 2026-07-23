Testsuite for (toplevel that loads plugins).

  $ make_toplevel_plugin_host dune-site.toplevel

  $ write_toplevel_plugin_sources
  $ make_toplevel_plugin 1
  $ make_toplevel_plugin 2

  $ dune build @install
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

