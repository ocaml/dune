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
