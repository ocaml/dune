  $ cat > dune-project << EOF
  > (lang dune 3.10)
  > 
  > (package
  >  (name mylib))
  > EOF

  $ cat > dune << EOF
  > (library
  >  (public_name mylib))
  > EOF

  $ cat > mylib.ml << EOF
  > (** This is the main module for mylib *)
  > 
  > (** A simple type definition *)
  > type t = int
  > 
  > (** A function that adds one *)
  > val add_one : int -> int
  > let add_one x = x + 1
  > 
  > module SubModule = struct
  >   (** A nested module *)
  >   type nested = string
  > end
  > EOF

  $ cat > mylib.mli << EOF
  > (** This is the main module for mylib *)
  > 
  > (** A simple type definition *)
  > type t = int
  > 
  > (** A function that adds one *)
  > val add_one : int -> int
  > 
  > module SubModule : sig
  >   (** A nested module *)
  >   type nested = string
  > end
  > EOF

  $ list_markdown_docs () {
  >   find _build/default/_doc/_markdown -name '*.md' | sort
  > }

Build markdown documentation:

  $ dune build @doc-markdown
  $ list_markdown_docs
  _build/default/_doc/_markdown/index.md
  _build/default/_doc/_markdown/mylib/Mylib-SubModule.md
  _build/default/_doc/_markdown/mylib/Mylib.md
  _build/default/_doc/_markdown/mylib/index.md

Check the top-level index contains markdown:

  $ cat _build/default/_doc/_markdown/index.md
  # OCaml Package Documentation
  
  - [mylib](mylib/index.md)

Test that @doc still generates HTML as usual:

  $ dune build @doc
  $ find _build/default/_doc/_html -name '*.html' | sort
  _build/default/_doc/_html/index.html
  _build/default/_doc/_html/mylib/Mylib/SubModule/index.html
  _build/default/_doc/_html/mylib/Mylib/index.html
  _build/default/_doc/_html/mylib/index.html
