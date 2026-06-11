When %{bin:foo} occurs in a bash action, it may be assumed to be runnable which
was the case until after 3.23. In 3.24 %{bin:foo} would expand to the build
path rather than the install staging path.

This meant that binaries in the same directory are indistiguishable from
commands, i.e. things to be looked up in PATH.

  $ make_dune_project 3.24
  $ cat >> dune-project <<EOF
  > (package (name foopkg))
  > EOF

  $ cat > dune <<EOF
  > (executable (public_name foo) (package foopkg))
  > (rule
  >  (alias runtest)
  >  (action (bash %{bin:foo})))
  > EOF

  $ cat >foo.ml <<'EOF'
  > let () = print_endline "hello from foo"
  > EOF

The following does not work because bash is looking up foo.exe in PATH rather
than executing ./foo.exe directly.

CR-soon Alizter: We should expand as ./ so that bash and other tools don't have
to fall into this trap.

The exact bash error message varies across systems, so we strip the
bash-prefixed line and keep only the dune-side context.

  $ dune build @runtest 2>&1 | grep -v '/bash:\|^bash:'
  File "dune", lines 2-4, characters 0-51:
  2 | (rule
  3 |  (alias runtest)
  4 |  (action (bash %{bin:foo})))
  [1]
