A lock-dir package depends on another lock-dir package that depends on a
workspace package installing an executable. The workspace executable is
materialised into the package install layout and action expansion can resolve
`(run foo)` from that transitive dependency.

  $ make_dune_project 3.24
  $ cat >> dune-project <<EOF
  > (package
  >  (name ws-tool)
  >  (version 1.2.3))
  > EOF

  $ mkdir src
  $ cat > src/dune <<EOF
  > (executable
  >  (name main)
  >  (package ws-tool)
  >  (public_name foo))
  > EOF
  $ cat > src/main.ml <<EOF
  > let () = print_endline "from-workspace"
  > EOF

  $ make_lockdir
  $ make_lockpkg provider <<EOF
  > (version 0.0.1)
  > (depends ws-tool)
  > EOF
  $ make_lockpkg consumer <<EOF
  > (version 0.0.1)
  > (depends provider)
  > (build
  >  (progn
  >   (run foo)
  >   (system "test -x %{pkg:ws-tool:bin}/foo && echo bin-var-ok")
  >   (run echo %{pkg:ws-tool:name})
  >   (run echo %{pkg:ws-tool:version})
  >   (run echo %{pkg:ws-tool:dev})))
  > EOF

  $ write_lockdir_consumer_rule

The build succeeds. The package action expander resolves `foo`, package section
variables, and standard package metadata through the transitive workspace
dependency.

  $ dune build out
  from-workspace
  bin-var-ok
  ws-tool
  1.2.3
  false
