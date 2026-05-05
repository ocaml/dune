%{bin:...} in a cross-compilation context. The pform expands via the
host context's artifacts, and the install bin dir on the action's
PATH is the host's install bin dir.

mybin is gated to the host context only. If cross-context resolution
were broken, the binary wouldn't be found and the action would fail.

  $ cat >dune-project <<EOF
  > (lang dune 3.24)
  > (package (name mypkg))
  > EOF

  $ cat >dune-workspace <<EOF
  > (lang dune 3.24)
  > (context (default (name host)))
  > (context (default (name target) (host host)))
  > EOF

  $ cat >dune <<'EOF'
  > (executable
  >  (public_name mybin)
  >  (package mypkg)
  >  (enabled_if (= %{context_name} host)))
  > (rule
  >  (enabled_if (= %{context_name} target))
  >  (deps %{bin:mybin})
  >  (action
  >   (progn
  >    (with-stdout-to path-output
  >     (bash "echo $PATH"))
  >    (run mybin))))
  > EOF

  $ cat >mybin.ml <<'EOF'
  > let () = print_endline "hello"
  > EOF

  $ dune build _build/target/path-output
  hello

  $ env_added "$(cat _build/target/path-output)" "$PATH"
  $TESTCASE_ROOT/_build/install/host/bin
