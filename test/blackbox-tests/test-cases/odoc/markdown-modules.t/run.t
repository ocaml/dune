Test that markdown generation includes all modules following naming conventions.

  $ cat > dune-project << EOF
  > (lang dune 3.0)
  > (package (name mylib))
  > EOF

  $ cat > dune << EOF
  > (library
  >  (public_name mylib))
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

  $ cat _build/default/_doc/_markdown/mylib/Mylib.md
  
  # Module `Mylib`
  
  ```
  module Main : sig ... end
  ```
  ```
  module Main_nested : sig ... end
  ```
  ```
  module Main_nested_deep : sig ... end
  ```
  ```
  module Main_sub : sig ... end
  ```
