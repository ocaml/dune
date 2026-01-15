Dune itself produces action traces

  $ cat >dune-project<<EOF
  > (lang dune 3.22)
  > EOF

  $ cat >dune <<EOF
  > (rule (with-stdout-to foo (echo bar)))
  > (rule
  >  (alias foo)
  >  (deps (source_tree .))
  >  (action (run dune build ./foo)))
  > EOF

  $ dune build @foo

  $ dune trace cat | jq 'include "dune";
  >   select(.name == "init")
  > | { args: .args | keys, keys: keys, argv: .args.argv | .[1:] }
  > '
  {
    "args": [
      "argv",
      "build_dir",
      "env",
      "initial_cwd",
      "pid",
      "root"
    ],
    "keys": [
      "args",
      "cat",
      "name",
      "ts"
    ],
    "argv": [
      "build",
      "@foo"
    ]
  }
  {
    "args": [
      "argv",
      "build_dir",
      "digest",
      "env",
      "initial_cwd",
      "pid",
      "root"
    ],
    "keys": [
      "args",
      "cat",
      "name",
      "ts"
    ],
    "argv": [
      "build",
      "./foo"
    ]
  }
