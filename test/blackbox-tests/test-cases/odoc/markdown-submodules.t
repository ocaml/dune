Test that markdown generation includes all modules following naming conventions.

  $ cat > dune-project << EOF
  > (lang dune 3.0)
  > (package (name mylib))
  > EOF

  $ cat > dune << EOF
  > (library
  >  (public_name mylib)
  >  (modules main main_sub main_nested main_nested_deep))
  > EOF

Create separate module files following a naming convention:

  $ cat > main.mli << EOF
  > (** Main module *)
  > val x : int
  > EOF

  $ cat > main.ml << EOF
  > let x = 42
  > EOF

  $ cat > main_sub.mli << EOF
  > (** Main sub module *)
  > val y : string
  > EOF

  $ cat > main_sub.ml << EOF
  > let y = "hello"
  > EOF

  $ cat > main_nested.mli << EOF
  > (** Main nested module *)
  > val z : bool
  > EOF

  $ cat > main_nested.ml << EOF
  > let z = true
  > EOF

  $ cat > main_nested_deep.mli << EOF
  > (** Main nested deep module *)
  > val w : float
  > EOF

  $ cat > main_nested_deep.ml << EOF
  > let w = 3.14
  > EOF

Build the library and generate docs:

  $ dune build @doc-markdown

Check what markdown files were generated:

  $ find _build/default/_doc/_markdown -name "*.md" | sort
  _build/default/_doc/_markdown/index.md
  _build/default/_doc/_markdown/mylib/Mylib-Main.md
  _build/default/_doc/_markdown/mylib/Mylib-Main_nested.md
  _build/default/_doc/_markdown/mylib/Mylib-Main_nested_deep.md
  _build/default/_doc/_markdown/mylib/Mylib-Main_sub.md
  _build/default/_doc/_markdown/mylib/Mylib.md
  _build/default/_doc/_markdown/mylib/index.md

Great! All modules are being generated as separate files.
Let's verify that the content is correct:

  $ head -5 _build/default/_doc/_markdown/mylib/Mylib-Main_sub.md
  
  # Module `Mylib.Main_sub`
  
  ```
  val y : string
