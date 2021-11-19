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
  >  (action (with-stdout-to out.log (run %{bin:c} "c-plugins-b.b"))))
  > EOF

  $ cat >c/c_register.ml <<EOF
  > let registered : string list ref = ref []
  > EOF

  $ cat >c/c.ml <<EOF
  > let () = try Sites.Plugins.Plugins.load Sys.argv.(1)
  >          with exn -> print_endline (Printexc.to_string exn)
  > let () = Printf.printf "run c: registered:%s.\n%!" (String.concat "," !C_register.registered)
  > EOF

  $ cat > dune-project << EOF
  > (lang dune 2.2)
  > EOF

Build everything
----------------

  $ dune build

Test with dune exec
--------------------------------
  $ dune exec -- c/c.exe "c-plugins-b.b"
  run b
  run c: registered:b.

Test error messages
--------------------------------
  $ dune exec -- c/c.exe "inexistent"
  The plugin "inexistent" can't be found in the search paths "$TESTCASE_ROOT/_build/install/default/lib/c/plugins".
  run c: registered:.

  $ cat >c/c.ml <<EOF
  > let l = Lazy.force Dune_site.Private_.Helpers.ocamlpath
  > let l = List.map (Printf.sprintf "OCAMLPATH=%s") l
  > let () = print_string (String.concat ":" l)
  > EOF

  $ export BUILD_PATH_PREFIX_MAP="$(dune exe -- c/c.exe):$BUILD_PATH_PREFIX_MAP"

  $ cat >c/c.ml <<EOF
  > let () = try Dune_site_plugins.V1.load Sys.argv.(1)
  >          with exn -> print_endline (Printexc.to_string exn)
  > EOF

  $ dune exec -- c/c.exe "inexistent" 2>&1 | sed -e 's&default/lib:.*&default/lib:..."&g'
  The library "inexistent" can't be found in the search paths "$TESTCASE_ROOT/_build/install/default/lib:..."

  $ dune exec -- c/c.exe "b.b.b"
  run b

  $ dune exec -- c/c.exe "b.b.inexistent" 2>&1  | sed -e 's&default/lib:.*&default/lib:..."&g'
  The sub-library "inexistent" can't be found in the library b.b in the search paths "$TESTCASE_ROOT/_build/install/default/lib:..."
