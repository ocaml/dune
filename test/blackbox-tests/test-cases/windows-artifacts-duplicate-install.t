Reproduction case for Windows artifacts duplicate key crash
https://github.com/ocaml/dune/issues/12535

When an install stanza contains both `name.exe` and `name` (e.g., the
executable and a wrapper script), The Windows handling in Dune strips the
`.exe` suffix in artifacts.ml, causing both to become "name", leading to
duplicate keys in the artifacts map.

  $ cat > dune-project <<EOF
  > (lang dune 3.21)
  > (package
  >  (name myapp))
  > EOF

  $ cat > dune <<EOF
  > (executable
  >  (name myapp)
  >  (public_name myapp))
  > 
  > (install
  >  (section bin)
  >  (files
  >   myapp.exe
  >   myapp))
  > EOF

  $ cat > myapp.ml <<EOF
  > let () = print_endline "Hello from myapp"
  > EOF

Wrapper script:
  $ cat > myapp <<'EOF'
  > #!/bin/sh
  > exec $(dirname $0)/myapp.exe "$@"
  > EOF
  $ chmod +x myapp

On Windows, this should not crash with duplicate "myapp" keys.

  $ dune build myapp.install 2>&1 | grep "Internal"
  Internal error, please report upstream including the contents of _build/log.

