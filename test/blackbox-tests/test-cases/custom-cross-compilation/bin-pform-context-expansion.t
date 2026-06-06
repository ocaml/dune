%{bin:...} in a cross-compilation context. The pform expands via the
host context's artifacts, and the action's PATH gets the .binaries
dir and the install bin dir, both under the host context.

  $ make_dune_project_with_package 3.24 mypkg

  $ cat >dune-workspace <<EOF
  > (lang dune 3.24)
  > (context (default (name host)))
  > (context (default (name target) (host host)))
  > EOF

  $ cat >dune <<'EOF'
  > (executable (public_name mybin) (package mypkg))
  > (rule
  >  (enabled_if (= %{context_name} target))
  >  (deps %{bin:mybin})
  >  (action
  >   (with-stdout-to path-output
  >    (bash "echo $PATH"))))
  > EOF

  $ cat >mybin.ml <<'EOF'
  > let () = print_endline "hello"
  > EOF

  $ dune build _build/target/path-output

  $ env_added "$(cat _build/target/path-output)" "$PATH" | censor
  $PWD/_build/install/host/.binaries/$DIGEST
