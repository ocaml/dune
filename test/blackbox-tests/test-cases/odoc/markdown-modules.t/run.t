Test that markdown generation includes all modules following naming conventions.

  $ cat > dune-project << EOF
  > (lang dune 3.0)
  > (package (name mylib))
  > EOF

  $ cat > dune << EOF
  > (library
  >  (public_name mylib))
  > EOF

  $ dune build @doc-markdown

  $ find _build/default/_doc/_markdown -name "*.md" | sort
  _build/default/_doc/_markdown/index.md
  _build/default/_doc/_markdown/mylib/Mylib-Main.md
  _build/default/_doc/_markdown/mylib/Mylib-Main_nested.md
  _build/default/_doc/_markdown/mylib/Mylib-Main_nested_deep.md
  _build/default/_doc/_markdown/mylib/Mylib-Main_sub.md
  _build/default/_doc/_markdown/mylib/Mylib.md
  _build/default/_doc/_markdown/mylib/index.md

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
