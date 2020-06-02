Check the expansion of %{dir:...}

  $ touch dune-workspace

  $ mkdir -p a/b/c

  $ cat > a/b/dune-project <<EOF
  > (lang dune 2.5)
  > EOF

  $ cat > a/b/dune <<EOF
  > (rule
  >  (alias foo)
  >  (action (chdir %{workspace_root} (echo "%{dir:c}\n"))))
  > EOF

  $ dune build @foo
  a/b/c
