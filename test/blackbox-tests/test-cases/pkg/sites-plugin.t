Test sites plugins from another package

  $ . ./helpers.sh
  $ mkrepo
  $ add_mock_repo_if_needed

Make an executable using dune-site (example mostly from the manual)
  $ mkdir app
  $ cd app
  $ cat > dune-project <<EOF
  > (lang dune 3.20)
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
  >  (package app)
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
  > (generate_opam_files true)
  > 
  > (package
  >  (depends app)
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
  > 
  > (rule
  >  (alias runtest)
  >  (action
  >   (run app)))
  > EOF
  $ cat > plugin/plugin1_impl.ml <<EOF
  > let () =
  > print_endline "Registration of Plugin1";
  > Queue.add (fun () -> print_endline "Plugin1 is doing something...") Registration.todo
  > EOF
  $ dune build @all 2>&1 | dune_cmd sanitize
  $ dune build @runtest 2>&1 | dune_cmd sanitize
  Registration of Plugin1
  Main app starts...
  Plugin1 is doing something...
  $ cd ..
  $ tar cf app.tar app
  $ rm -rf app

Configure our fake curl to serve the tarball:

  $ echo app.tar >> fake-curls
  $ PORT=1

Make a package for the executable and the plugin:
  $ mkpkg app <<EOF
  > build: [
  >   [
  >     "dune"
  >     "build"
  >     "-p"
  >     name
  >   ]
  > ]
  > url {
  >  src: "http://0.0.0.0:$PORT"
  >  checksum: [
  >   "md5=$(md5sum app.tar | cut -f1 -d' ')"
  >  ]
  > }
  > EOF
  $ mkpkg plugin1 <<EOF
  > depends: [
  >   "app"
  > ]
  > build: [
  >   [
  >     "dune"
  >     "build"
  >     "-p"
  >     name
  >   ]
  > ]
  > url {
  >  src: "http://0.0.0.0:$PORT"
  >  checksum: [
  >   "md5=$(md5sum app.tar | cut -f1 -d' ')"
  >  ]
  > }
  > EOF

Make a project that uses the executable in a dune rule.
We should observe the same behaviour that when running test above.

  $ cat > dune-project <<EOF
  > (lang dune 3.20)
  > (package
  >  (name foo)
  >  (depends
  >   (app :with-test)
  >   (plugin1 :with-test)))
  > EOF
  $ cat > foo.ml <<EOF
  > let foo = ()
  > EOF
  $ cat > dune <<EOF
  > (library
  >  (public_name foo))
  > 
  > (rule
  >  (alias runtest)
  >  (action
  >   (run app)))
  > EOF

  $ dune pkg lock
  Solution for dune.lock:
  - app.0.0.1
  - plugin1.0.0.1
  $ dune build @runtest 2>&1 | dune_cmd sanitize
  Main app starts...

Should have printed:
```
Registration of Plugin1
Main app starts...
Plugin1 is doing something...
```
