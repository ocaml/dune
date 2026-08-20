Regression test for #12996: dune exec doesn't perform install stanza,
so binaries using dune-site can't find their static files.

  $ cat > dune-project <<EOF
  > (lang dune 3.20)
  > (using dune_site 0.1)
  > (name mre)
  > (package
  >  (name mre)
  >  (depends ocaml)
  >  (sites (share static)))
  > EOF

  $ cat > dune <<'EOF'
  > (executable
  >  (public_name mre)
  >  (name main)
  >  (libraries dune-site)
  >  (modules main sites))
  > (generate_sites_module
  >  (module sites)
  >  (sites mre))
  > (install
  >  (section (site (mre static)))
  >  (files foo.txt))
  > EOF

  $ echo "hello from static" > foo.txt

  $ cat > main.ml <<'EOF'
  > module Sites = Sites.Sites
  > let () =
  >   let static_dir = List.hd Sites.static in
  >   let path = Filename.concat static_dir "foo.txt" in
  >   if Sys.file_exists path then
  >     In_channel.with_open_text path (fun ic ->
  >       match In_channel.input_line ic with
  >       | Some s -> print_endline s
  >       | None -> print_endline "empty file")
  >   else
  >     Printf.printf "File not found: %s\n" path
  > EOF

Running via dune exec should find the static file:

  $ dune exec -- mre
  File not found: $TESTCASE_ROOT/_build/install/default/share/mre/static/foo.txt
