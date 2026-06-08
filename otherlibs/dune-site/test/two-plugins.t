Test sites plugins with two plugins

The tests for one plugin should be allowed to run in parallel with the build of
other plugins.

We test that by creating two plugins:
* plugin1 contains a test that depends on plugin1 and the app,
* plugin2 is similar, but artificially slow to build.

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


  $ mkdir plugin1
  $ cat > plugin1/dune-project <<EOF
  > (lang dune 3.8)
  > (using dune_site 0.1)
  > 
  > (generate_opam_files true)
  > 
  > (package
  >  (name plugin1))
  > EOF

  $ cat > plugin1/dune <<EOF
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
  > 
  > (rule
  >  (alias runtest)
  >  (deps
  >    (package plugin1)
  >    %{bin:app})
  >  (action
  >   (run app)))
  > EOF

  $ cat > plugin1/plugin1_impl.ml <<EOF
  > let () =
  > print_endline "Registration of Plugin1";
  > Queue.add (fun () -> print_endline "Plugin1 is doing something...") Registration.todo
  > EOF


  $ mkdir plugin2
  $ cat > plugin2/dune-project <<EOF
  > (lang dune 3.8)
  > (using dune_site 0.1)
  > 
  > (generate_opam_files true)
  > 
  > (package
  >  (name plugin2))
  > EOF

  $ cat > plugin2/dune <<EOF
  > (library
  >  (public_name plugin2.plugin2_impl)
  >  (name plugin2_impl)
  >  (modules plugin2_impl answer)
  >  (libraries app.register))
  > 
  > (plugin
  >  (name plugin2)
  >  (libraries plugin2.plugin2_impl)
  >  (site (app plugins)))
  > 
  > (rule
  >  (progn
  >   (run sleep 1)
  >   (write-file answer.ml "let x = 42")))
  > EOF

  $ cat > plugin2/plugin2_impl.ml <<EOF
  > let () =
  > print_endline "Registration of Plugin2";
  > Queue.add (fun () -> Printf.printf "Plugin2 computed the answer: %d\n" Answer.x) Registration.todo
  > EOF


  $ dune build @install @check @runtest 2>&1 | grep -o 'Fatal error: exception Dynlink.Error (Dynlink.Cannot_open_dll'
  Fatal error: exception Dynlink.Error (Dynlink.Cannot_open_dll

The full error message explains that `Cannot_open_dll` is triggered because
`_build/install/default/lib/plugin2/plugin2_impl/plugin2_impl.cmxs` cannot be
found when the test is run. The message changes slightly depending on the
platform.
