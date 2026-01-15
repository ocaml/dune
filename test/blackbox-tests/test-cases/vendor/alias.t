Test vendor stanza with library aliasing

  $ cat > dune-project << EOF
  > (lang dune 3.22)
  > EOF

  $ mkdir -p oldlib.1.0.0

Create a vendored library (directly in oldlib.1.0.0, not vendor/oldlib.1.0.0):
  $ cat > oldlib.1.0.0/dune-project << EOF
  > (lang dune 3.22)
  > (name oldlib)
  > (package (name oldlib))
  > EOF

  $ cat > oldlib.1.0.0/dune << EOF
  > (library
  >  (name oldlib)
  >  (public_name oldlib))
  > EOF

  $ cat > oldlib.1.0.0/oldlib.ml << EOF
  > let version = "1.0.0"
  > EOF

Use vendor stanza with aliasing (oldlib :as newlib):
The wrapper module changes to the alias name, so access is Newlib.Oldlib.<fn>
  $ cat > dune << EOF
  > (vendor oldlib.1.0.0 (libraries (oldlib :as newlib)))
  > (executable
  >  (name main)
  >  (libraries newlib))
  > EOF

Note: The wrapper module is Newlib (from the alias), with the original module
inside: Newlib.Oldlib.version
  $ cat > main.ml << EOF
  > let () = print_endline ("Version: " ^ Newlib.Oldlib.version)
  > EOF

Build and run:
  $ dune exec ./main.exe
  Version: 1.0.0
