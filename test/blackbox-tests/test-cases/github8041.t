  $ cat > dune-project << EOF
  > (lang dune 1.0)
  > (package
  >  (name p))
  > EOF

  $ cat > dune << EOF
  > (install
  >  (files data.txt)
  >  (section share))
  > EOF

  $ echo contents > data.txt

If sendfile fails, we should fallback to the portable implementation.

  $ strace -e inject=sendfile:error=EINVAL -o /dev/null dune build @install
