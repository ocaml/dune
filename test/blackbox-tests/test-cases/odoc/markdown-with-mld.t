Test markdown generation with package documentation (.mld files)

  $ cat > dune-project << EOF
  > (lang dune 3.10)
  > (package
  >  (name example))
  > EOF

  $ cat > dune << EOF
  > (library
  >  (public_name example))
  > EOF

  $ cat > example.ml << EOF
  > (** Example library module *)
  > 
  > let greet name = Printf.sprintf "Hello, %s!" name
  > EOF

  $ cat > index.mld << EOF
  > {0 Example Package}
  > 
  > This is the documentation for the example package.
  > 
  > {1 Overview}
  > 
  > This package provides a simple greeting function.
  > 
  > {2 Usage}
  > 
  > {[
  > let message = Example.greet "World"
  > ]}
  > 
  > See {!Example} for the API documentation.
  > EOF

  $ dune build @doc-markdown

  $ find _build/default/_doc/_markdown -name '*.md' | sort
  _build/default/_doc/_markdown/example/Example.md
  _build/default/_doc/_markdown/example/index.md
  _build/default/_doc/_markdown/index.md

  $ ls _build/default/_doc/_markdown/example/
  Example.md
  index.md

  $ dune build @doc-markdown

  $ dune build @doc @doc-markdown
  $ find _build/default/_doc -name 'index.*' | grep -E '(html|md)$' | sort
  _build/default/_doc/_html/example/Example/index.html
  _build/default/_doc/_html/example/index.html
  _build/default/_doc/_html/index.html
  _build/default/_doc/_markdown/example/index.md
  _build/default/_doc/_markdown/index.md
