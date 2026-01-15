Test conflicting aliases - two vendor stanzas aliasing to the same name

  $ cat > dune-project << EOF
  > (lang dune 3.22)
  > EOF

Create two different vendored libraries:

  $ mkdir -p lib_a.1.0.0 lib_b.1.0.0

  $ cat > lib_a.1.0.0/dune-project << EOF
  > (lang dune 3.22)
  > (package (name lib_a))
  > EOF

  $ cat > lib_a.1.0.0/dune << EOF
  > (library
  >  (name lib_a)
  >  (public_name lib_a))
  > EOF

  $ cat > lib_a.1.0.0/lib_a.ml << EOF
  > let msg = "from lib_a"
  > EOF

  $ cat > lib_b.1.0.0/dune-project << EOF
  > (lang dune 3.22)
  > (package (name lib_b))
  > EOF

  $ cat > lib_b.1.0.0/dune << EOF
  > (library
  >  (name lib_b)
  >  (public_name lib_b))
  > EOF

  $ cat > lib_b.1.0.0/lib_b.ml << EOF
  > let msg = "from lib_b"
  > EOF

Both libraries aliased to the same name:
  $ cat > dune << EOF
  > (vendor lib_a.1.0.0 (libraries (lib_a :as shared_name)))
  > (vendor lib_b.1.0.0 (libraries (lib_b :as shared_name)))
  > (executable
  >  (name main)
  >  (libraries shared_name))
  > EOF

  $ cat > main.ml << EOF
  > let () = print_endline "test"
  > EOF

  $ dune build main.exe 2>&1 | head -10
  File "lib_a.1.0.0/dune", lines 1-3, characters 0-44:
  1 | (library
  2 |  (name lib_a)
  3 |  (public_name lib_a))
  Error: Library with name "shared_name" is already defined in
  lib_b.1.0.0/dune:1. Either change one of the names, or enable them
  conditionally using the 'enabled_if' field.
  [1]
