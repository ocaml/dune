Test vendor stanza with package field referencing non-existent package

  $ cat > dune-project << EOF
  > (lang dune 3.22)
  > EOF

  $ mkdir -p mylib.1.0.0
  $ cat > mylib.1.0.0/dune-project << EOF
  > (lang dune 3.22)
  > (package (name mylib))
  > EOF

  $ cat > mylib.1.0.0/dune << EOF
  > (library
  >  (name mylib)
  >  (public_name mylib))
  > EOF

  $ cat > mylib.1.0.0/mylib.ml << EOF
  > let msg = "hello"
  > EOF

Test: vendor stanza with non-existent package name
The package "nonexistent" doesn't exist in mylib.1.0.0, so no libraries match.

  $ cat > dune << EOF
  > (vendor mylib.1.0.0 (package nonexistent))
  > (executable
  >  (name main)
  >  (libraries mylib))
  > EOF

  $ cat > main.ml << EOF
  > let () = print_endline Mylib.msg
  > EOF

  $ dune build main.exe 2>&1 | head -5
  File "dune", line 4, characters 12-17:
  4 |  (libraries mylib))
                  ^^^^^
  Error: Library "mylib" not found.
  -> required by _build/default/.main.eobjs/native/dune__exe__Main.cmx
  [1]
