Sandbox a rule that depends on a directory target using the copying sandbox
mode:

  $ cat >dune-project <<EOF
  > (lang dune 3.8)
  > (using directory-targets 0.1)
  > EOF

  $ cat >dune <<EOF
  > (rule
  >  (targets (dir output))
  >  (action (bash "mkdir -p output/; echo x > output/x; echo y > output/y")))
  > (rule
  >  (target foo)
  >  (deps (sandbox always) output/)
  >  (action
  >   (run touch foo)))
  > EOF

  $ dune build foo --sandbox=copy
  Error: Is a directory
  -> required by _build/default/foo
  [1]
