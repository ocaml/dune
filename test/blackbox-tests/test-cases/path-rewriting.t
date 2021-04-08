Observe that dune currently doesn't help commands that understand
BUILD_PATH_PREFIX_MAP to rewrite the current working directory:

  $ echo '(lang dune 3.0)' > dune-project
  $ cat > dune <<"EOF"
  > (rule
  >  (target x)
  >  (action (system "dune_cmd rewrite-path $PWD; touch x")))
  > EOF

  $ dune build
            sh x
  $TESTCASE_ROOT/_build/default

Which is particularly bad when the action is sandboxed:

  $ dune build --sandbox copy
            sh x
  $TESTCASE_ROOT/_build/.sandbox/7e88b37a209ffa243f77f6aed0d26f12/default
