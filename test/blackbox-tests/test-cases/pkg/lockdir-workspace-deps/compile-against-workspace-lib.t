A lock-dir package depends on a workspace library and its build
action inspects [OCAMLPATH].

  $ make_workspace_lib_package

The lock dir contains one package "consumer" whose build action
writes [OCAMLPATH] to a file and installs it so the test can later
inspect what consumer saw.

  $ make_lockdir
  $ make_lockpkg consumer <<EOF
  > (version 0.0.1)
  > (depends workspace-lib)
  > (build
  >  (system "echo \$OCAMLPATH > ocamlpath.txt"))
  > (install
  >  (system "mkdir -p %{lib}/consumer && cp ocamlpath.txt %{lib}/consumer/"))
  > EOF

A rule depends on the lock-dir package:

  $ write_lockdir_consumer_rule

The build succeeds. Consumer's build action sees the workspace install
layout prepended to OCAMLPATH. Print just the first OCAMLPATH entry:

  $ dune build out

  $ find _build -name 'ocamlpath.txt' -exec cat {} \; \
  >   | awk -F: '{print $1}' | censor
  $PWD/_build/install/default/.packages/$DIGEST/lib
