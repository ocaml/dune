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

  $ cat > top_with_plugins.ml <<EOF
  > let main () =
  >   print_endline "\nMain app really starts...";
  >   (* load all the available plugins *)
  >   Sites.Plugins.Top_plugins.load_all ();
  >   print_endline "Main app after loading plugins...";
  >   (* Execute the code registered by the plugins *)
  >   print_endline "Main app executing registered plugins...";
  >   Queue.iter (fun f -> f ()) Registration.todo;
  >   print_endline "Main app after executing registered plugins...";
  >   exit (Topmain.main ())
  > 
  > let () =
  >     main()
  > EOF

  $ cat > registration.ml <<EOF
  > let todo : (unit -> unit) Queue.t = Queue.create ()
  > let register f =
  >   print_endline "In register";
  >   Queue.add f todo;
  >   print_endline "Done in register";
  > EOF

  $ mkdir plugin1
  $ cat > plugin1/dune-project <<EOF
  > (lang dune 3.7)
  > (using dune_site 0.1)
  > 
  > (generate_opam_files true)
  > (wrapped_executables false)
  > (map_workspace_root false)
  > (package
  >  (name top-plugin1))
  > EOF

  $ cat > plugin1/dune <<EOF
  > (library
  >  (public_name top-plugin1.plugin1_impl)
  >  (modes byte)
  >  (name plugin1_impl)
  >  (modules plugin1_impl)
  >  (libraries top_with_plugins.register))
  > 
  > (plugin
  >  (name plugin1)
  >  (libraries top-plugin1.plugin1_impl)
  >  (site (top_with_plugins top_plugins)))
  > EOF

  $ cat > plugin1/plugin1_impl.ml <<EOF
  > let myfun () =
  >   print_endline "Plugin1 is doing something..."
  > 
  > let () =
  >   print_endline "Registration of Plugin1";
  >   Registration.register myfun;
  >   print_endline "Done with registration of Plugin1";
  > EOF

  $ mkdir plugin2
  $ cat > plugin2/dune-project <<EOF
  > (lang dune 3.7)
  > (using dune_site 0.1)
  > 
  > (generate_opam_files true)
  > (wrapped_executables false)
  > (map_workspace_root false)
  > (package
  >  (name top-plugin2))
  > EOF

  $ cat > plugin2/dune <<EOF
  > (library
  >  (public_name top-plugin2.plugin2_impl)
  >  (modes byte)
  >  (name plugin2_impl)
  >  (modules plugin2_impl)
  >  (libraries top_with_plugins.register))
  > 
  > (plugin
  >  (name plugin2)
  >  (libraries top-plugin2.plugin2_impl)
  >  (site (top_with_plugins top_plugins)))
  > EOF

  $ cat > plugin2/plugin2_impl.ml <<EOF
  > let myfun () =
  >   print_endline "Plugin2 is doing something..."
  > 
  > let () =
  >   print_endline "Registration of Plugin2";
  >   Registration.register myfun;
  >   print_endline "Done with registration of Plugin2";
  > EOF

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

