Check that Dune help commands that understand BUILD_PATH_PREFIX_MAP to
rewrite the current working directory:

  $ echo '(lang dune 3.0)' > dune-project
  $ cat > dune <<"EOF"
  > (rule
  >  (target x)
  >  (action (system "dune_cmd rewrite-path $PWD; touch x")))
  > EOF

  $ dune build
            sh x
  /workspace_root

It works with sandboxing as well:

  $ dune clean
  $ dune build --sandbox copy
            sh x
  /workspace_root
