  $ mkdir -p b c

  $ for i in b; do
  > mkdir -p $i
  > cat >$i/dune-project <<EOF
  > (lang dune 3.0)
  > (using dune_site 0.1)
  > (name $i)
  > (package (name $i) (depends c))
  > EOF
  > done

  $ for i in c; do
  >   mkdir -p $i
  >   cat >$i/dune-project <<EOF
  > (lang dune 3.0)
  > (using dune_site 0.1)
  > (name $i)
  > (package (name $i) (sites (share data) (lib plugins)))
  > EOF
  > done

  $ cat >b/dune <<EOF
  > (library
  >  (public_name b.b.b)
  >  (name b)
  >  (libraries c.register dune-site))
  > (generate_sites_module (module sites) (sites b))
  > (plugin (name c-plugins-b.b) (libraries b.b.b) (site (c plugins)))
  > EOF

  $ cat >b/b.ml <<EOF
  > let v = "b"
  > let () = Printf.printf "run b\n%!"
  > let () = C_register.registered := "b"::!C_register.registered
  > EOF

  $ cat >c/dune <<EOF
  > (executable
  >  (public_name c)
  >  (promote (until-clean))
  >  (modules c sites)
  >  (libraries c.register dune-site dune-site.plugins))
  > (library
  >  (public_name c.register)
  >  (name c_register)
  >  (modules c_register))
  > (generate_sites_module (module sites) (plugins (c plugins)))
  > (rule
  >  (targets out.log)
  >  (deps (package c))
  >  (action (with-stdout-to out.log (run %{bin:c}))))
  > EOF

  $ cat >c/c_register.ml <<EOF
  > let registered : string list ref = ref []
  > EOF

  $ cat >c/c.ml <<EOF
  > let () = Sites.Plugins.Plugins.load "c-plugins-b.b"
  > let () = Printf.printf "run c: registered:%s.\n%!" (String.concat "," !C_register.registered)
  > EOF

  $ cat > dune-project << EOF
  > (lang dune 2.2)
  > EOF

Build everything
----------------

  $ dune build
  File "c/dune", line 11, characters 0-95:
  11 | (rule
  12 |  (targets out.log)
  13 |  (deps (package c))
  14 |  (action (with-stdout-to out.log (run %{bin:c}))))
  Fatal error: exception Dune_site_plugins__Plugins.Library_not_found("c-plugins-b.b")
  [1]

Test with dune exec
--------------------------------
  $ dune exec -- c/c.exe
  Fatal error: exception Dune_site_plugins__Plugins.Library_not_found("c-plugins-b.b")
  [2]
