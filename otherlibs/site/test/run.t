Test embedding of sites locations information
-----------------------------------

  $ mkdir -p a b c

  $ for i in a b d; do
  > mkdir -p $i
  > cat >$i/dune-project <<EOF
  > (lang dune 2.8)
  > (using dune_site 0.1)
  > (name $i)
  > (package (name $i) (sites (share data)))
  > EOF
  > done

  $ for i in c; do
  >   mkdir -p $i
  >   cat >$i/dune-project <<EOF
  > (lang dune 2.8)
  > (using dune_site 0.1)
  > (name $i)
  > (package (name $i) (sites (share data) (lib plugins)))
  > EOF
  > done

  $ cat >a/dune <<EOF
  > (library
  >  (public_name a)
  >  (libraries dune-site))
  > (generate_sites_module (module sites) (sites a))
  > EOF

  $ cat >a/a.ml <<EOF
  > let v = "a"
  > let () = Printf.printf "run a\n%!"
  > let () = List.iter (Printf.printf "a: %s\n%!") Sites.Sites.data
  > EOF

  $ cat >b/dune <<EOF
  > (library
  >  (public_name b.b.b)
  >  (name b)
  >  (libraries c.register dune-site))
  > (generate_sites_module (module sites) (sites b))
  > (plugin (name c-plugins-b) (libraries b.b.b) (site (c plugins)))
  > (install (section (site (b data))) (files info.txt))
  > EOF

  $ cat >b/b.ml <<EOF
  > let v = "b"
  > let () = Printf.printf "run b\n%!"
  > let () = C_register.registered := "b"::!C_register.registered
  > let () = List.iter (Printf.printf "b: %s\n%!") Sites.Sites.data
  > let () =
  >     let test d = Sys.file_exists (Filename.concat d "info.txt") in
  >     let found = List.exists test Sites.Sites.data in
  >     Printf.printf "info.txt is found: %b\n%!" found
  > EOF

  $ cat >b/info.txt <<EOF
  > Lorem
  > EOF

  $ cat >d/dune <<EOF
  > (library
  >  (public_name d)
  >  (libraries c.register dune-site non-existent-library)
  >  (optional))
  > (generate_sites_module (module sites) (sites d))
  > (plugin (name c-plugins-d) (libraries d) (site (c plugins)) (optional))
  > (install (section (site (d data))) (files info.txt))
  > EOF

  $ cat >d/d.ml <<EOF
  > let v = "d"
  > let () = Printf.printf "run d\n%!"
  > let () = C_register.registered := "d"::!C_register.registered
  > let () = List.iter (Printf.printf "d: %s\n%!") Sites.Sites.data
  > let () =
  >     let test d = Sys.file_exists (Filename.concat d "info.txt") in
  >     let found = List.exists test Sites.Sites.data in
  >     Printf.printf "info.txt is found: %d\n%!" found
  > EOF

  $ cat >d/info.txt <<EOF
  > Lorem
  > EOF


  $ cat >c/dune <<EOF
  > (executable
  >  (public_name c)
  >  (promote (until-clean))
  >  (modules c sites)
  >  (libraries a c.register dune-site dune-site.plugins))
  > (library
  >  (public_name c.register)
  >  (name c_register)
  >  (modules c_register))
  > (generate_sites_module (module sites) (sourceroot) (plugins (c plugins)))
  > (rule
  >  (targets out.log)
  >  (deps (package c))
  >  (action (with-stdout-to out.log (run %{bin:c}))))
  > EOF

  $ cat >c/c_register.ml <<EOF
  > let registered : string list ref = ref []
  > EOF

  $ cat >c/c.ml <<EOF
  > let () = Printf.printf "run c: %s linked registered:%s.\n%!"
  >   A.v (String.concat "," !C_register.registered)
  > let () = match Sites.sourceroot with
  >       | Some d -> Printf.printf "sourceroot is %S\n%!" d
  >       | None -> Printf.printf "no sourceroot\n%!"
  > let () = List.iter (Printf.printf "c: %s\n%!") Sites.Sites.data
  > let () = Printf.printf "b is available: %b\n%!" (Dune_site_plugins.V1.available "b")
  > let () = Sites.Plugins.Plugins.load_all ()
  > let () = Printf.printf "run c: registered:%s.\n%!" (String.concat "," !C_register.registered)
  > EOF

  $ cat > dune-project << EOF
  > (lang dune 2.2)
  > EOF

  $ dune build

Test with a normal installation
--------------------------------

  $ dune install --prefix _install 2> /dev/null

Once installed, we have the sites information:

  $ OCAMLPATH=_install/lib:$OCAMLPATH _install/bin/c
  run a
  a: $TESTCASE_ROOT/_install/share/a/data
  run c: a linked registered:.
  no sourceroot
  c: $TESTCASE_ROOT/_install/share/c/data
  b is available: true
  run b
  b: $TESTCASE_ROOT/_install/share/b/data
  info.txt is found: true
  run c: registered:b.

Test with a relocatable installation
--------------------------------

  $ dune install --prefix _install_relocatable --relocatable 2> /dev/null

Once installed, we have the sites information:

  $ _install_relocatable/bin/c
  run a
  a: $TESTCASE_ROOT/_install_relocatable/share/a/data
  run c: a linked registered:.
  no sourceroot
  c: $TESTCASE_ROOT/_install_relocatable/share/c/data
  b is available: true
  run b
  b: $TESTCASE_ROOT/_install_relocatable/share/b/data
  info.txt is found: true
  run c: registered:b.

Test after moving a relocatable installation
--------------------------------

  $ mv _install_relocatable  _install_relocatable2

Once installed, we have the sites information:

  $ _install_relocatable2/bin/c
  run a
  a: $TESTCASE_ROOT/_install_relocatable2/share/a/data
  run c: a linked registered:.
  no sourceroot
  c: $TESTCASE_ROOT/_install_relocatable2/share/c/data
  b is available: true
  run b
  b: $TESTCASE_ROOT/_install_relocatable2/share/b/data
  info.txt is found: true
  run c: registered:b.

Test substitution when promoting
--------------------------------

It is wrong that info.txt is not found, but to make it work it is an important
development because b is not promoted

  $ c/c.exe
  run a
  a: $TESTCASE_ROOT/_build/install/default/share/a/data
  run c: a linked registered:.
  sourceroot is "$TESTCASE_ROOT"
  c: $TESTCASE_ROOT/_build/install/default/share/c/data
  b is available: true
  run b
  info.txt is found: false
  run c: registered:b.

Test within dune rules
--------------------------------
  $ dune build c/out.log

  $ cat _build/default/c/out.log
  run a
  a: $TESTCASE_ROOT/_build/install/default/share/a/data
  run c: a linked registered:.
  sourceroot is "$TESTCASE_ROOT"
  c: $TESTCASE_ROOT/_build/install/default/share/c/data
  b is available: true
  run b
  b: $TESTCASE_ROOT/_build/install/default/share/b/data
  info.txt is found: true
  run c: registered:b.


Test with dune exec
--------------------------------
  $ dune exec -- c/c.exe
  run a
  a: $TESTCASE_ROOT/_build/install/default/share/a/data
  run c: a linked registered:.
  sourceroot is "$TESTCASE_ROOT"
  c: $TESTCASE_ROOT/_build/install/default/share/c/data
  b is available: true
  run b
  b: $TESTCASE_ROOT/_build/install/default/share/b/data
  info.txt is found: true
  run c: registered:b.


Test compiling an external plugin
---------------------------------
  $ mkdir e
  $ cat >e/dune-project <<EOF
  > (lang dune 2.8)
  > (using dune_site 0.1)
  > (name e)
  > (package (name e) (sites (share data)))
  > EOF

  $ cat >e/dune <<EOF
  > (library
  >  (public_name e)
  >  (libraries c.register dune-site))
  > (generate_sites_module (module sites) (sites e))
  > (plugin (name c-plugins-e) (libraries e) (site (c plugins)))
  > (install (section (site (e data))) (files info.txt))
  > EOF

  $ cat >e/e.ml <<EOF
  > let v = "e"
  > let () = Printf.printf "run e\n%!"
  > let () = C_register.registered := "e"::!C_register.registered
  > let () = List.iter (Printf.printf "e: %s\n%!") Sites.Sites.data
  > let () =
  >     let test d = Sys.file_exists (Filename.concat d "info.txt") in
  >     let found = List.exists test Sites.Sites.data in
  >     Printf.printf "info.txt is found: %b\n%!" found
  > EOF

  $ cat >e/info.txt <<EOF
  > Lorem
  > EOF

  $ OCAMLPATH=$(pwd)/_install/lib:$OCAMLPATH dune build --root=e
  Entering directory 'e'

  $ OCAMLPATH=$(pwd)/_install/lib:$OCAMLPATH PATH=$(pwd)/_install/bin:$PATH dune exec  --root=e -- c
  Entering directory 'e'
  run a
  a: $TESTCASE_ROOT/_install/share/a/data
  run c: a linked registered:.
  no sourceroot
  c: $TESTCASE_ROOT/_install/share/c/data
  b is available: true
  run b
  b: $TESTCASE_ROOT/_install/share/b/data
  info.txt is found: true
  run e
  e: $TESTCASE_ROOT/e/_build/install/default/share/e/data
  info.txt is found: true
  run c: registered:e,b.

  $ OCAMLPATH=$(pwd)/_install/lib:$OCAMLPATH dune install --root=e --prefix $(pwd)/_install 2> /dev/null

  $ OCAMLPATH=_install/lib:$OCAMLPATH _install/bin/c
  run a
  a: $TESTCASE_ROOT/_install/share/a/data
  run c: a linked registered:.
  no sourceroot
  c: $TESTCASE_ROOT/_install/share/c/data
  b is available: true
  run b
  b: $TESTCASE_ROOT/_install/share/b/data
  info.txt is found: true
  run e
  e: $TESTCASE_ROOT/_install/share/e/data
  info.txt is found: true
  run c: registered:e,b.
