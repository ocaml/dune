When mdx is used to generate a program, the program will read the preludes at
run time. This test makes sure that this is recorded as a dependency.

  $ cat > dune << EOF
  > (mdx (preludes prelude.ml))
  > EOF

  $ cat > dune-project << EOF
  > (lang dune 3.6)
  > (using mdx 0.3)
  > EOF

  $ cat > prelude.ml << EOF
  > let foo () = 1
  > EOF

  $ cat > README.md << 'EOF'
  > ```ocaml
  > # foo ();;
  > - : int = 1
  > ```
  > EOF

  $ dune runtest

  $ echo 'let foo () = 2' > prelude.ml

  $ dune runtest --auto-promote
  File "README.md", line 1, characters 0-0:
  --- README.md
  +++ .mdx/README.md.corrected
  @@ -1,4 +1,4 @@
   ```ocaml
   # foo ();;
  -- : int = 1
  +- : int = 2
   ```
  Promoting _build/default/.mdx/README.md.corrected to README.md.
  [1]

  $ cat README.md
  ```ocaml
  # foo ();;
  - : int = 2
  ```
