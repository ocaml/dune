Test that dune-site plugins load correctly in development mode (without
installing or using --release). This tests the path substitution mechanism
that embeds plugin locations into the executable.

Setup a minimal project with a main app and a plugin:

  $ cat > dune-project <<EOF
  > (lang dune 3.8)
  > (using dune_site 0.1)
  > (name app)
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
  > let plugins_loaded : string list ref = ref []
  > EOF

  $ cat > app.ml <<EOF
  > let () = Sites.Plugins.Plugins.load_all ()
  > let () =
  >   match !Registration.plugins_loaded with
  >   | [] -> print_endline "ERROR: No plugins loaded!"
  >   | plugins ->
  >     print_endline "Plugins loaded:";
  >     List.iter (fun p -> print_endline ("  - " ^ p)) plugins
  > EOF

Create a plugin:

  $ mkdir plugin
  $ cat > plugin/dune-project <<EOF
  > (lang dune 3.8)
  > (using dune_site 0.1)
  > (generate_opam_files true)
  > (package (name plugin1))
  > EOF

  $ cat > plugin/dune <<EOF
  > (library
  >  (public_name plugin1.impl)
  >  (name plugin1_impl)
  >  (modules plugin1_impl)
  >  (libraries app.register))
  > 
  > (plugin
  >  (name plugin1)
  >  (libraries plugin1.impl)
  >  (site (app plugins)))
  > EOF

  $ cat > plugin/plugin1_impl.ml <<EOF
  > let () =
  >   print_endline "Plugin1 registering...";
  >   Registration.plugins_loaded := "plugin1" :: !Registration.plugins_loaded
  > EOF

Build and run via dune exec - plugins should load in dev mode:

  $ dune build @all 2>&1 | dune_cmd sanitize
  $ dune exec -- ./app.exe
  Plugin1 registering...
  Plugins loaded:
    - plugin1
