When link_flags are set in the env stanza, they should be passed to the
mdx_gen linker invocation.

  $ make_mdx_project 3.0 0.2

  $ cat > README.md << 'EOF'
  > ```ocaml
  > # 1 + 1;;
  > - : int = 2
  > ```
  > EOF

  $ cat > dune << 'EOF'
  > (mdx)
  > EOF

  $ cat > dune-workspace << 'EOF'
  > (lang dune 3.0)
  > (env (dev (link_flags (:standard -cclib -lm))))
  > EOF

  $ dune build

Show that the env link_flags (-cclib -lm) are missing from the mdx_gen linker
invocation.

  $ dune trace cat | jq_dune -r '
  >   processes
  >   | select(any(.args.target_files[]?; contains("mdx_gen")))
  >   | .args.process_args[]
  >   | select(. == "-cclib" or . == "-lm")
  > '
