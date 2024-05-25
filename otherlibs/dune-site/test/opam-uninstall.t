Test an approximation of an OPAM install / uninstall of a plugin package

This takes the sites-plugin.t blackbox test extracted from the manual and
changes its end to cover:
- dune install of both the application and its plugin,
- dune uninstall of the plugin, leaving an empty directory as OPAM would.

  $ cat > dune-project <<EOF
  > (lang dune 3.8)
  > (using dune_site 0.1)
  > (name app)
  > 
  > (package
  >  (name app)
  >  (sites (lib plugins)))
  > EOF

  $ cat > dune <<EOF
  > (executable
  >  (public_name app)
  >  (modules sites app)
  >  (libraries app.register dune-site dune-site.plugins))
  > 
  > (library
  >  (public_name app.register)
  >  (name registration)
  >  (modules registration))
  > 
  > (generate_sites_module
  >  (module sites)
  >  (plugins (app plugins)))
  > EOF

  $ cat > registration.ml <<EOF
  > let todo : (unit -> unit) Queue.t = Queue.create ()
  > EOF

  $ cat > app.ml <<EOF
  > (* load all the available plugins *)
  > let () = Sites.Plugins.Plugins.load_all ()
  > 
  > let () = print_endline "Main app starts..."
  > (* Execute the code registered by the plugins *)
  > let () = Queue.iter (fun f -> f ()) Registration.todo
  > EOF


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

  $ cat > plugin/dune <<EOF
  > (library
  >  (public_name plugin1.plugin1_impl)
  >  (name plugin1_impl)
  >  (modules plugin1_impl)
  >  (libraries app.register))
  > 
  > (plugin
  >  (name plugin1)
  >  (libraries plugin1.plugin1_impl)
  >  (site (app plugins)))
  > EOF

  $ cat > plugin/plugin1_impl.ml <<EOF
  > let () =
  > print_endline "Registration of Plugin1";
  > Queue.add (fun () -> print_endline "Plugin1 is doing something...") Registration.todo
  > EOF

  $ dune build @install
  $ dune install --prefix _install

  $ OCAMLPATH=_install/lib:$OCAMLPATH _install/bin/app
  Registration of Plugin1
  Main app starts...
  Plugin1 is doing something...

  $ dune uninstall --prefix _install plugin1

Unfortunately, the fact that the `lib/app/plugins/plugin1` directory should be
removed along with plugin1 is lost in the OPAM metadata, so we simulate this
issue by recreating this empty directory.

  $ mkdir -p _install/lib/app/plugins/plugin1

  $ OCAMLPATH=_install/lib:$OCAMLPATH _install/bin/app
  Main app starts...

