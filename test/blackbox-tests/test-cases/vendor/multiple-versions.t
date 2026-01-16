Test cherry-picking packages from different versions of the same repository.

This simulates a cohttp-like structure where you want cohttp from 6.0.0
but cohttp-lwt from 6.1.0.

  $ cat > dune-project << EOF
  > (lang dune 3.22)
  > EOF

Create two versions of the same multi-package repository:

  $ mkdir -p vendor/myrepo.1.0.0 vendor/myrepo.2.0.0

Version 1.0.0:
  $ cat > vendor/myrepo.1.0.0/dune-project << EOF
  > (lang dune 3.22)
  > (package (name core))
  > (package (name extra))
  > EOF

  $ cat > vendor/myrepo.1.0.0/dune << EOF
  > (library
  >  (name core)
  >  (modules core)
  >  (public_name core))
  > (library
  >  (name extra)
  >  (modules extra)
  >  (libraries core)
  >  (public_name extra))
  > EOF

  $ cat > vendor/myrepo.1.0.0/core.ml << EOF
  > let version = "1.0.0"
  > EOF

  $ cat > vendor/myrepo.1.0.0/extra.ml << EOF
  > let info = "extra from " ^ Core.version
  > EOF

Version 2.0.0:
  $ cat > vendor/myrepo.2.0.0/dune-project << EOF
  > (lang dune 3.22)
  > (package (name core))
  > (package (name extra))
  > EOF

  $ cat > vendor/myrepo.2.0.0/dune << EOF
  > (library
  >  (name core)
  >  (modules core)
  >  (public_name core))
  > (library
  >  (name extra)
  >  (modules extra)
  >  (libraries core)
  >  (public_name extra))
  > EOF

  $ cat > vendor/myrepo.2.0.0/core.ml << EOF
  > let version = "2.0.0"
  > EOF

  $ cat > vendor/myrepo.2.0.0/extra.ml << EOF
  > let info = "extra from " ^ Core.version
  > EOF

Cherry-pick: core from 1.0.0, extra from 2.0.0
  $ cat > vendor/dune << EOF
  > (vendor myrepo.1.0.0 (package core))
  > (vendor myrepo.2.0.0 (package extra))
  > EOF

  $ cat > dune << EOF
  > (executable
  >  (name main)
  >  (libraries core extra))
  > EOF

  $ cat > main.ml << EOF
  > let () =
  >   print_endline ("core version: " ^ Core.version);
  >   print_endline ("extra says: " ^ Extra.info)
  > EOF

Cherry-picking works. The workspace sees:
- core from 1.0.0
- extra from 2.0.0 (which uses core from 1.0.0)

  $ dune exec ./main.exe
  core version: 1.0.0
  extra says: extra from 1.0.0
