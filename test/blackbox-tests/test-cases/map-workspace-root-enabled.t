Check that Dune does not do rewriting of build directory to /workspace_root
if inhibited.

  $ cat > dune-project << EOF
  > (lang dune 3.7)
  > (map_workspace_root true)
  > EOF
  $ cat > dune << "EOF"
  > (rule
  >  (target x)
  >  (action (system "dune_cmd rewrite-path $PWD | grep -c /workspace_root; touch x")))
  > EOF

  $ dune build
  1

It works with sandboxing as well:

  $ dune clean
  $ dune build --sandbox copy
  1
