%{bin:NAME} as a (run ...) target invokes the binary; as a deps
entry it adds a .binaries dir to the action's PATH.

  $ make_dune_project_with_package 3.24 mypkg
  $ cat >dune <<'EOF'
  > (executable (public_name mybin) (package mypkg))
  > (rule
  >  (deps %{bin:mybin})
  >  (action
  >   (progn
  >    (with-stdout-to path-output
  >     (bash "echo $PATH"))
  >    (run %{bin:mybin}))))
  > EOF
  $ cat >mybin.ml <<'EOF'
  > let () = print_endline "hello from mybin"
  > EOF

  $ dune build path-output
  hello from mybin
  $ env_added "$(cat _build/default/path-output)" "$PATH" | censor
  $PWD/_build/install/default/.binaries/$DIGEST

The rule's deps include the build artifact and the .binaries
symlink:

  $ dune rules --format=json _build/default/path-output \
  >   | jq_dune '.[] | ruleDepFilePaths' \
  >   | grep mybin | censor
  "_build/default/mybin.exe"
  "_build/install/default/.binaries/$DIGEST/mybin"

Building the same target again pointlessly invalidates and rebuilds the
.binaries symlink:

  $ DUNE_TRACE=cache dune build path-output
  $ dune trace cat \
  >   | jq_dune -s 'cacheMissesMatching("\\.binaries")' \
  >   | censor
  {
    "name": "workspace_local_miss",
    "target": "_build/install/default/.binaries/$DIGEST/mybin",
    "reason": "target missing from build dir"
  }
  {
    "name": "miss",
    "target": "_build/install/default/.binaries/$DIGEST/mybin",
    "reason": "can't go in shared cache"
  }
