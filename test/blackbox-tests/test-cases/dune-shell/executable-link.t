Replay uses Dune's action interpreter, not a shell rendering of the action.
Linking an executable exercises a real internal rule with arguments and paths
that cannot usefully be reconstructed by the test.

  $ make_dune_project 3.23

  $ cat > dune <<'EOF'
  > (executable
  >  (name main))
  > EOF

  $ cat > main.ml <<'EOF'
  > let () = print_endline "linked by replay"
  > EOF

An internal compilation target lives below its object directory, but its
prepared action runs from the context's source directory.  The shell follows
the action's outer mapped [chdir], not the target directory.

  $ dune shell --sandbox=copy \
  >   _build/default/.main.eobjs/byte/dune__exe__Main.cmo -- sh -c 'printf "compiler-cwd: "; echo "$PWD"' | censor
  compiler-cwd: $PWD/_build/.sandbox/$DIGEST/default

The target is absent at the intercepted action boundary. Its prerequisites
have already been built and materialized in the canonical action sandbox.
Changing the source after entry therefore does not cause replay to run another
build: the already-prepared link action still succeeds, and can be run again.

  $ ROOT=$PWD dune shell --sandbox=copy _build/default/main.exe -- sh -c '
  > printf "cwd: "; echo "$PWD"
  > printf "%s\n" "$PWD" > "$ROOT/execution-path"
  > test ! -e main.exe && echo "before: absent"
  > printf "this is not valid OCaml\n" > "$ROOT/main.ml"
  > "$DUNE_SHELL/dune-run"
  > test -x main.exe && echo "first: executable"
  > ./main.exe
  > "$DUNE_SHELL/dune-run"
  > test -x main.exe && echo "second: executable"
  > ./main.exe
  > ' | censor
  cwd: $PWD/_build/.sandbox/$DIGEST/default
  before: absent
  first: executable
  linked by replay
  second: executable
  linked by replay

The ordinary [Sandbox.with_] lifetime ends with the shell child. The canonical
sandbox is removed, and replay output is not extracted into the real build
directory.

  $ if test -e "$(cat execution-path)"; then
  >   echo "sandbox-after-shell: present"
  > else
  >   echo "sandbox-after-shell: removed"
  > fi
  sandbox-after-shell: removed

  $ if test -e _build/default/main.exe; then
  >   echo "build target: present"
  > else
  >   echo "build target: absent"
  > fi
  build target: absent
