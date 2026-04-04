Mdx tests run in a sandbox, so undeclared files are not visible.

  $ cat > dune-project << EOF
  > (lang dune 3.22)
  > (using mdx 0.5)
  > EOF

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

  $ cat > dune-project << EOF
  > (lang dune 3.22)
  > (using mdx 0.4)
  > EOF

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
