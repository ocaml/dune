Replay uses Dune's action interpreter, not a shell rendering of the action.
Linking an executable exercises a real internal rule with arguments and paths
that cannot usefully be reconstructed by the test.

  $ cat > dune-project <<'EOF'
  > (lang dune 3.23)
  > EOF

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
  >   _build/default/.main.eobjs/byte/dune__exe__Main.cmo -- sh -c '
  > expected="$(cat "$DUNE_SHELL/sandbox")/default"
  > if test "$PWD" = "$expected"; then
  >   echo "compiler-cwd: exact mapped action directory"
  > else
  >   echo "compiler-cwd: unexpected: $PWD"
  > fi
  > ' 2>compiler-entry.stderr
  compiler-cwd: exact mapped action directory

The target is absent at the intercepted action boundary. Its prerequisites
have already been built and materialized in the canonical action sandbox.
Changing the source after entry therefore does not cause replay to run another
build: the already-prepared link action still succeeds, and can be run again.

  $ ROOT=$PWD dune shell --sandbox=copy _build/default/main.exe -- sh -c '
  > expected="$(cat "$DUNE_SHELL/sandbox")/default"
  > if test "$PWD" = "$expected"; then
  >   echo "cwd: exact mapped directory"
  > else
  >   echo "cwd: unexpected: $PWD"
  > fi
  > printf "%s\n" "$PWD" > "$ROOT/execution-path"
  > if test -e main.exe; then
  >   echo "before: present"
  > else
  >   echo "before: absent"
  > fi
  > printf "this is not valid OCaml\n" > "$ROOT/main.ml"
  > "$DUNE_SHELL/dune-run"
  > if test -x main.exe; then
  >   echo "first: executable"
  > else
  >   echo "first: missing"
  > fi
  > ./main.exe
  > "$DUNE_SHELL/dune-run"
  > if test -x main.exe; then
  >   echo "second: executable"
  > else
  >   echo "second: missing"
  > fi
  > ./main.exe
  > ' 2>shell-entry.stderr
  cwd: exact mapped directory
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
