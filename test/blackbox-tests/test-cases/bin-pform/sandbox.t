%{bin:NAME} deps under sandboxing.

dune's sandbox does not relocate the PATH environment variable. The
action's CWD is sandboxed, but PATH still points at the un-sandboxed
bin-layout dir. This works locally because absolute filesystem paths
are still readable from inside the sandbox. It would break under
remote action execution.

  $ cat >dune-project <<EOF
  > (lang dune 3.24)
  > (package (name mypkg))
  > EOF
  $ mkdir src
  $ cat >src/dune <<'EOF'
  > (executable (public_name mybin) (package mypkg))
  > EOF
  $ cat >src/mybin.ml <<'EOF'
  > let () = print_endline "hello from mybin"
  > EOF

  $ cat >dune <<'EOF'
  > (rule
  >  (deps (sandbox always) %{bin:mybin})
  >  (action
  >   (with-stdout-to out
  >    (bash "pwd; command -v mybin; mybin"))))
  > EOF

The action runs in the sandbox; pwd is sandboxed but the resolved
mybin path is the un-sandboxed bin-layout dir:

  $ dune build out --sandbox symlink 2>&1
  $ cat _build/default/out | censor
  $PWD/_build/.sandbox/$DIGEST1/default
  $PWD/_build/install/default/.binaries/$DIGEST2/mybin
  hello from mybin

The rule's deps include the build artifact and the bin-layout
symlink. Both are paths under the un-sandboxed _build/:

  $ dune rules --format=json _build/default/out \
  >   | jq 'include "dune"; .[] | ruleDepFilePaths' \
  >   | grep mybin | censor
  "_build/default/src/mybin.exe"
  "_build/install/default/.binaries/$DIGEST/mybin"
