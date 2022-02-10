Dune should know about the fact that mdx reads the MDX_RUN_NON_DETERMINISTIC
variable. When using the stanza 0.2, it is the mdx driver that reads that
variable. This is only the case since mdx 2.1.0.

  $ cat > dune-project << EOF
  > (lang dune 3.0)
  > (using mdx 0.2)
  > EOF

  $ cat > dune << EOF
  > (mdx (files README.md))
  > EOF

  $ cat > README.md << 'EOF'
  > ```ocaml
  > # "a";;
  > ```
  > 
  > ```ocaml non-deterministic
  > # "b";;
  > ```
  > EOF

  $ dune runtest --auto-promote
  File "README.md", line 1, characters 0-0:
  Error: Files _build/default/README.md and
  _build/default/.mdx/README.md.corrected differ.
  Promoting _build/default/.mdx/README.md.corrected to README.md.
  [1]

  $ cat README.md
  ```ocaml
  # "a";;
  - : string = "a"
  ```
  
  ```ocaml non-deterministic
  # "b";;
  ```

  $ dune runtest

  $ MDX_RUN_NON_DETERMINISTIC=1 dune runtest --auto-promote
  File "README.md", line 1, characters 0-0:
  Error: Files _build/default/README.md and
  _build/default/.mdx/README.md.corrected differ.
  Promoting _build/default/.mdx/README.md.corrected to README.md.
  [1]

  $ cat README.md
  ```ocaml
  # "a";;
  - : string = "a"
  ```
  
  ```ocaml non-deterministic
  # "b";;
  - : string = "b"
  ```
