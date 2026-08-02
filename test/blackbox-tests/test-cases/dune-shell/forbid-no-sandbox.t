For an ordinary rule with no command-line sandbox override, dune selects
[none]. The action's actual execution location is the real build directory.
The shell uses that location and warns that writes affect the build directly.

  $ cat > dune-project <<'EOF'
  > (lang dune 3.2)
  > EOF

  $ cat > dune <<'EOF'
  > (rule
  >  (target prepared-input)
  >  (deps source-input)
  >  (action (with-stdout-to %{target} (cat source-input))))
  > 
  > (rule
  >  (target no-sandbox)
  >  (deps prepared-input)
  >  (action (with-stdout-to %{target} (cat prepared-input))))
  > 
  > (rule
  >  (target unrelated)
  >  (action (with-stdout-to %{target} (echo visible-in-real-build))))
  > EOF

  $ echo prepared > source-input
  $ dune build _build/default/unrelated _build/default/no-sandbox

  $ ROOT=$PWD dune shell _build/default/no-sandbox -- sh -c '
  > case "$PWD" in
  >   "$ROOT"/_build/default) echo "directory: real build" ;;
  >   *) echo "directory: unexpected: $PWD" ;;
  > esac
  > if test -e no-sandbox; then
  >   echo "declared-target-on-entry: visible"
  > else
  >   echo "declared-target-on-entry: cleared"
  > fi
  > printf "undeclared-build-target: "; cat unrelated; echo
  > printf "prepared-dependency: "; cat prepared-input
  > printf "changed after entry\n" > "$ROOT/source-input"
  > "$DUNE_SHELL/dune-run"
  > printf "target: "; cat no-sandbox
  > ' >shell.stdout 2>shell.stderr
  $ cat shell.stdout
  directory: real build
  declared-target-on-entry: cleared
  undeclared-build-target: visible-in-real-build
  prepared-dependency: prepared
  target: prepared

Replay under [none] writes directly into the configured build directory. The
target therefore remains there after the live shell action returns; there is
no sandbox root to extract or destroy. The changed source was not rebuilt by
replay.

  $ printf "target-after-shell: "; cat _build/default/no-sandbox
  target-after-shell: prepared
  $ printf "prepared-dependency-after-shell: "; cat _build/default/prepared-input
  prepared-dependency-after-shell: prepared

An interactive session under [none] warns in its help screen that this is the
real build directory and that commands write there directly.

  $ dune shell _build/default/no-sandbox <<'EOF' 2>/dev/null
  > . "$DUNE_SHELL/init.bash" >/dev/null 2>&1
  > help | grep -qi 'sandbox mode none' && echo "help-mode: none"
  > help | grep -qi 'real _build' && echo "help-mode: warns real build"
  > EOF
  help-mode: none
  help-mode: warns real build

A source-copy rule reads directly from the source tree.  Its prepared path
must round-trip through the editable action even though replay starts in the
real build directory.

  $ echo direct-source > direct-source
  $ export TEST_SHELL="$(command -v sh)"
  $ export TEST_CAT="$(command -v cat)"
  $ dune shell _build/default/direct-source -- "$TEST_SHELL" -c '
  > if test "$PWD" = "$1/_build/default"; then
  >   echo "source-copy-cwd: exact"
  > else
  >   echo "source-copy-cwd: unexpected: $PWD"
  > fi
  > "$DUNE_SHELL/dune-run"
  > printf "source-copy-replay: "; "$0" direct-source
  > ' "$TEST_CAT" "$PWD" 2>source-copy.stderr
  source-copy-cwd: exact
  source-copy-replay: direct-source
  $ printf "source-copy-after-shell: "; cat _build/default/direct-source
  source-copy-after-shell: direct-source
