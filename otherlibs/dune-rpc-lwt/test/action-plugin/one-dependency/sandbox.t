Dynamically requested dependencies must be made available in the running sandbox.
The client requests dependencies from a subdirectory, concurrently and repeatedly,
then reads an overlapping glob and an empty directory. Static inputs modified by
an earlier part of the action must not be replaced by their original contents.
A directory target requested after one of its files must not replace that file.

  $ cat > dune-project <<'EOF'
  > (lang dune 3.25)
  > (using action-plugin 0.1)
  > EOF
  $ mkdir inputs empty subdir
  $ cp bin/foo.exe .
  $ printf original > inputs/static.txt
  $ cat > dune <<'EOF'
  > (rule
  >  (target choice)
  >  (deps selection)
  >  (action (copy selection choice)))
  > (rule
  >  (target result)
  >  (deps inputs/static.txt (sandbox always))
  >  (action (chdir subdir (dynamic-run ../foo.exe sandbox))))
  > (rule
  >  (targets (dir tree))
  >  (action
  >   (progn
  >    (run mkdir -p tree/sub)
  >    (bash "printf first > tree/sub/first")
  >    (bash "printf second > tree/sub/second"))))
  > EOF
  $ cat > inputs/dune <<'EOF'
  > (rule
  >  (target picked.txt)
  >  (action (write-file %{target} picked)))
  > (rule
  >  (target other.txt)
  >  (action (write-file %{target} other)))
  > (rule
  >  (target unused)
  >  (action (bash "echo 'unmatched dependency was built'; exit 1")))
  > EOF

The second build should reuse the result. Changing a discovered dependency must
rerun the action with its new contents, without exposing unmatched dependencies.

  $ for mode in symlink hardlink; do
  >   echo "$mode:"
  >   printf ../inputs/picked.txt > selection
  >   if dune build result --sandbox "$mode" --build-dir "_build-$mode"; then
  >     cat "_build-$mode/default/result"
  >     dune build result --sandbox "$mode" --build-dir "_build-$mode"
  >     printf ../inputs/other.txt > selection
  >     dune build result --sandbox "$mode" --build-dir "_build-$mode"
  >     cat "_build-$mode/default/result"
  >     test ! -e "_build-$mode/default/inputs/unused" &&
  >       test "$(cat inputs/static.txt)" = original && echo 'checked isolation'
  >   else
  >     echo 'sandboxed build failed'
  >   fi
  > done 2>&1 | sed -E 's|\.sandbox/[a-f0-9]{32}|.sandbox/SANDBOX|g'
  symlink:
  starting sandboxed action
  picked
  other.txt, picked.txt, static.txt
  local
  starting sandboxed action
  other
  other.txt, picked.txt, static.txt
  local
  checked isolation
  hardlink:
  starting sandboxed action
  picked
  other.txt, picked.txt, static.txt
  local
  starting sandboxed action
  other
  other.txt, picked.txt, static.txt
  local
  checked isolation

An accepted request can outlive the plugin process. The generator acknowledges
that the request reached Dune, then waits for the plugin to exit. Disable
background sandbox operations so cleanup finishes before the generator's
completion is handled, without relying on a delay in the generator.

  $ cat > dune-project <<'EOF'
  > (lang dune 2.0)
  > (using action-plugin 0.1)
  > EOF
  $ export DUNE_CONFIG__BACKGROUND_SANDBOXES=disabled
  $ state=$(mktemp -d)
  $ export PLUGIN_STATE="$state"
  $ printf first > control
  $ cat > slow.sh <<'EOF'
  > if [ -f "$1/pid" ]; then
  >   touch "$1/started"
  >   while kill -0 "$(cat "$1/pid")" 2>/dev/null; do sleep 0.01; done
  > fi
  > cat control > slow-input
  > EOF
  $ cat > dune <<'EOF'
  > (rule
  >  (target slow-input)
  >  (deps control (sandbox none))
  >  (action (run sh %{dep:slow.sh} %{env:PLUGIN_STATE=unset})))
  > (rule
  >  (target detached)
  >  (deps (sandbox always))
  >  (action (dynamic-run ./foo.exe detached %{env:PLUGIN_STATE=unset})))
  > EOF
  $ cat > sandbox-config <<'EOF'
  > (lang dune 3.25)
  > (sandboxing_preference hardlink none)
  > EOF
  $ build_detached() {
  >   timeout 10 dune build -j 2 detached \
  >     --config-file sandbox-config --build-dir _build-lifetime
  > }
  $ build_detached
  ran

Accepted requests finish before the sandbox is destroyed.

  $ find _build-lifetime/.sandbox -name slow-input -exec echo leaked \;

Their dependencies are recorded in the cached action result.

  $ rm "$state/pid" "$state/started"
  $ printf second > control
  $ build_detached
  ran
  $ cat _build-lifetime/default/slow-input
  second
