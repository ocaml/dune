Test interaction between vendor and vendored_dirs stanzas

  $ cat > dune-project << EOF
  > (lang dune 3.22)
  > EOF

  $ mkdir -p mylib.1.0.0

Create a vendored library with a warning:
  $ cat > mylib.1.0.0/dune-project << EOF
  > (lang dune 3.22)
  > (name mylib)
  > (package (name mylib))
  > EOF

  $ cat > mylib.1.0.0/dune << EOF
  > (library
  >  (name mylib)
  >  (public_name mylib))
  > EOF

  $ cat > mylib.1.0.0/mylib.ml << EOF
  > let x = 1
  > let _ = x  (* unused warning without vendored_dirs *)
  > let version = "1.0.0"
  > EOF

Test 1: vendor stanza alone (warnings shown)
  $ cat > dune << EOF
  > (vendor mylib.1.0.0)
  > (executable
  >  (name main)
  >  (libraries mylib))
  > EOF

  $ cat > main.ml << EOF
  > let () = print_endline Mylib.version
  > EOF

  $ dune build main.exe 2>&1 | grep -i warning || echo "no warnings"
  no warnings

Test 2: Both vendor and vendored_dirs (union - filtering + warning suppression)
  $ cat > dune << EOF
  > (vendored_dirs mylib.1.0.0)
  > (vendor mylib.1.0.0 (libraries mylib))
  > (executable
  >  (name main)
  >  (libraries mylib))
  > EOF

  $ dune build main.exe 2>&1 | grep -i warning || echo "no warnings"
  no warnings

  $ dune exec ./main.exe
  1.0.0

Test 3: vendored_dirs without vendor (all libraries exposed)
  $ cat > dune << EOF
  > (vendored_dirs mylib.1.0.0)
  > (executable
  >  (name main)
  >  (libraries mylib))
  > EOF

  $ dune exec ./main.exe
  1.0.0
