We test that a directory target with only other subdirs can be
properly promoted.

  $ cat > dune-project <<EOF
  > (lang dune 3.16)
  > (using directory-targets 0.1)
  > EOF

  $ cat > dune <<EOF
  > (rule
  >  (targets (dir node_modules))
  >  (mode (promote (until-clean)))
  >  (action
  >    (progn
  >      (system "mkdir -p node_modules/node-cmake/")
  >      (system "touch    node_modules/node-cmake/.jshintrc")
  >      (system "mkdir -p node_modules/node-cmake/node_modules/ansi-regex")
  >      (system "touch    node_modules/node-cmake/node_modules/ansi-regex/index.js")
  >    )))
  > EOF

  $ dune build node_modules
  $ ls node_modules
  node-cmake
