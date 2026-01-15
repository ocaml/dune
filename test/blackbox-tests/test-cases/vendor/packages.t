Test vendor stanza with multiple packages (one stanza per package)

  $ cat > dune-project << EOF
  > (lang dune 3.22)
  > EOF

  $ mkdir -p vendor.1.0.0/lib_a vendor.1.0.0/lib_b

Create a vendored project with two packages in separate directories:
  $ cat > vendor.1.0.0/dune-project << EOF
  > (lang dune 3.22)
  > (name vendor)
  > (package (name pkg-a))
  > (package (name pkg-b))
  > EOF

  $ cat > vendor.1.0.0/lib_a/dune << EOF
  > (library
  >  (name lib_a)
  >  (public_name pkg-a.lib))
  > EOF

  $ cat > vendor.1.0.0/lib_a/lib_a.ml << EOF
  > let msg = "from lib_a"
  > EOF

  $ cat > vendor.1.0.0/lib_b/dune << EOF
  > (library
  >  (name lib_b)
  >  (public_name pkg-b.lib))
  > EOF

  $ cat > vendor.1.0.0/lib_b/lib_b.ml << EOF
  > let msg = "from lib_b"
  > EOF

Test 1: Multiple vendor stanzas, one per package
  $ cat > dune << EOF
  > (vendor vendor.1.0.0 (package pkg-a))
  > (vendor vendor.1.0.0 (package pkg-b))
  > (executable
  >  (name main)
  >  (libraries pkg-a.lib pkg-b.lib))
  > EOF

  $ cat > main.ml << EOF
  > let () = print_endline (Lib_a.msg ^ " and " ^ Lib_b.msg)
  > EOF

  $ dune exec ./main.exe
  from lib_a and from lib_b

Test 2: Only expose pkg-a
  $ cat > dune << EOF
  > (vendor vendor.1.0.0 (package pkg-a))
  > (executable
  >  (name main)
  >  (libraries pkg-a.lib))
  > EOF

  $ cat > main.ml << EOF
  > let () = print_endline Lib_a.msg
  > EOF

  $ dune exec ./main.exe
  from lib_a

Test 3: pkg-b should not be accessible when only pkg-a is exposed
  $ cat > dune << EOF
  > (vendor vendor.1.0.0 (package pkg-a))
  > (executable
  >  (name main)
  >  (libraries pkg-b.lib))
  > EOF

  $ dune build main.exe 2>&1 | head -5
  File "dune", line 4, characters 12-21:
  4 |  (libraries pkg-b.lib))
                  ^^^^^^^^^
  Error: Library "pkg-b.lib" not found.
  -> required by _build/default/.main.eobjs/native/dune__exe__Main.cmx
  [1]
