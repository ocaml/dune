Test error cases for vendor stanza

  $ cat > dune-project << EOF
  > (lang dune 3.22)
  > EOF

  $ mkdir -p mylib.1.0.0

Create a vendored library:
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
  > let version = "1.0.0"
  > EOF

Test 1: Vendor stanza referencing non-existent library
  $ cat > dune << EOF
  > (vendor mylib.1.0.0 (libraries nonexistent))
  > (executable
  >  (name main)
  >  (libraries mylib))
  > EOF

  $ cat > main.ml << EOF
  > let () = print_endline Mylib.version
  > EOF

The library mylib is not exposed because vendor stanza only allows nonexistent:
  $ dune build main.exe 2>&1 | head -10
  File "dune", line 4, characters 12-17:
  4 |  (libraries mylib))
                  ^^^^^
  Error: Library "mylib" not found.
  -> required by _build/default/.main.eobjs/native/dune__exe__Main.cmx
  -> required by _build/default/main.exe
  [1]

Test 2: Vendor stanza with :standard excluding non-existent library
  $ cat > dune << EOF
  > (vendor mylib.1.0.0 (libraries :standard \ nonexistent))
  > (executable
  >  (name main)
  >  (libraries mylib))
  > EOF

This should work - :standard includes mylib, and excluding nonexistent has no effect:
  $ dune exec ./main.exe
  1.0.0

Test 3: Vendor stanza for non-existent directory (silently ignored)
  $ cat > dune << EOF
  > (vendor nonexistent.1.0.0)
  > (executable
  >  (name main)
  >  (libraries mylib))
  > EOF

The vendor stanza for non-existent dir is silently ignored, so mylib is found normally:
  $ dune build main.exe 2>&1 | head -10

Test 4: Alias to non-existent library name (should work but be unused)
  $ cat > dune << EOF
  > (vendor mylib.1.0.0 (libraries (nonexistent :as something)))
  > (executable
  >  (name main)
  >  (libraries mylib))
  > EOF

The alias applies to nonexistent which doesn't exist, so mylib is excluded:
  $ dune build main.exe 2>&1 | head -10
  File "dune", line 4, characters 12-17:
  4 |  (libraries mylib))
                  ^^^^^
  Error: Library "mylib" not found.
  -> required by _build/default/.main.eobjs/native/dune__exe__Main.cmx
  -> required by _build/default/main.exe
  [1]

Test 5: Using aliased name correctly
  $ cat > dune << EOF
  > (vendor mylib.1.0.0 (libraries (mylib :as aliased)))
  > (executable
  >  (name main)
  >  (libraries aliased))
  > EOF

  $ cat > main.ml << EOF
  > let () = print_endline Aliased.Mylib.version
  > EOF

  $ dune exec ./main.exe
  1.0.0
