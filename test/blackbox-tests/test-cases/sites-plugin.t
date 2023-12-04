Test sites plugins (example from the manual)

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
  > (module sites)
  > (plugins (app plugins)))
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

  $ dune build --display short @all 2>&1 | dune_cmd sanitize
      ocamldep .app.eobjs/dune__exe__App.impl.d
        ocamlc .registration.objs/byte/registration.{cmi,cmo,cmt}
      ocamlopt .app.eobjs/native/dune_site__Dune_site_data.{cmx,o}
      ocamlopt .app.eobjs/native/dune_site_plugins__Dune_site_plugins_data.{cmx,o}
      ocamldep .app.eobjs/dune__exe__Sites.impl.d
      ocamlopt .registration.objs/native/registration.{cmx,o}
        ocamlc plugin/.plugin1_impl.objs/byte/plugin1_impl.{cmi,cmo,cmt}
        ocamlc registration.cma
        ocamlc .app.eobjs/byte/dune__exe.{cmi,cmo,cmt}
      ocamldep .app.eobjs/dune__exe__App.intf.d
      ocamlopt registration.{a,cmxa}
      ocamlopt plugin/.plugin1_impl.objs/native/plugin1_impl.{cmx,o}
        ocamlc plugin/plugin1_impl.cma
      ocamlopt .app.eobjs/native/dune__exe.{cmx,o}
        ocamlc .app.eobjs/byte/dune__exe__Sites.{cmi,cmo,cmt}
        ocamlc .app.eobjs/byte/dune__exe__App.{cmi,cmti}
      ocamlopt registration.cmxs
      ocamlopt plugin/plugin1_impl.{a,cmxa}
      ocamlopt .app.eobjs/native/dune__exe__Sites.{cmx,o}
      ocamlopt .app.eobjs/native/dune__exe__App.{cmx,o}
      ocamlopt plugin/plugin1_impl.cmxs
      ocamlopt app.exe
  $ dune exec ./app.exe
  Registration of Plugin1
  Main app starts...
  Plugin1 is doing something...

