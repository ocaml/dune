A package dependency's site data should take precedence over inherited
locations without hiding files that are only available there.

  $ write_package_site_test_project
  $ cat >> dune <<'EOF'
  > (rule
  >  (alias runtest)
  >  (deps (package foo))
  >  (action (run main foo.txt fallback.txt)))
  > EOF
  $ mkdir -p inherited/data encoder
  $ echo inherited > inherited/data/foo.txt
  $ echo fallback > inherited/data/fallback.txt
  $ cat > encoder/dune <<'EOF'
  > (executable (name locations) (libraries dune-site.private))
  > EOF
  $ cat > encoder/locations.ml <<'EOF'
  > let () =
  >   let entry =
  >     { Dune_site_private.package = "foo"
  >     ; section = Dune_section.Share
  >     ; dir = Sys.argv.(1)
  >     }
  >   in
  >   print_endline (Dune_site_private.encode_dune_dir_locations [entry])
  > EOF
  $ inherited=$(dune_cmd native-path "$PWD/inherited")
  $ locations=$(dune exec ./encoder/locations.exe "$inherited")
  $ DUNE_DIR_LOCATIONS="$locations" dune runtest
  foo.txt: inherited
  fallback.txt: fallback
