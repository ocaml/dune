Mdx tests run in a sandbox, so undeclared files are not visible.

  $ make_mdx_project 3.22 0.5

  $ cat > dune << EOF
  > (rule
  >  (target secret.txt)
  >  (action (with-stdout-to %{target} (echo "top secret"))))
  > 
  > (mdx (files README.md))
  > EOF

  $ cat > README.md << 'EOF'
  > ```sh
  > $ [ -f secret.txt ] && cat secret.txt || echo "secret missing"
  > ```
  > EOF

  $ dune build secret.txt
  $ cat _build/default/secret.txt
  top secret

  $ dune runtest
  File "README.md", line 1, characters 0-0:
  --- README.md
  +++ .mdx/README.md.corrected
  @@ -1,3 +1,4 @@
   ```sh
   $ [ -f secret.txt ] && cat secret.txt || echo "secret missing"
  +secret missing
   ```
  [1]

  $ make_mdx_project 3.22 0.4

  $ dune runtest
  File "README.md", line 1, characters 0-0:
  --- README.md
  +++ .mdx/README.md.corrected
  @@ -1,3 +1,4 @@
   ```sh
   $ [ -f secret.txt ] && cat secret.txt || echo "secret missing"
  +top secret
   ```
  [1]

The mdx generator is also sandboxed starting with version 0.6.

  $ mdx_command_is_sandboxed() {
  >   dune trace cat | jq_dune -sc --arg command "$1" '
  >     [ .[]
  >     | processes
  >     | select(.args.prog | basename == "ocaml-mdx")
  >     | select(.args.process_args[0] == $command)
  >     | (.args.dir | contains(".sandbox"))
  >     ][0]'
  > }

Use a stable markdown file when testing the dependency scanner.

  $ cat > README.md <<'EOF'
  > # Sandboxing
  > EOF

  $ make_mdx_project 3.25 0.5
  $ dune build mdx_gen.ml-gen
  $ mdx_command_is_sandboxed dune-gen
  false
  $ dune runtest
  $ mdx_command_is_sandboxed deps
  false

  $ make_mdx_project 3.25 0.6
  $ dune build mdx_gen.ml-gen
  $ mdx_command_is_sandboxed dune-gen
  true
  $ echo >> README.md
  $ dune runtest
  $ mdx_command_is_sandboxed deps
  false
