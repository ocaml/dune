Relative reads use the client's current directory for both dependency requests
and I/O. Clients translate requests into Dune's root-relative namespace.

  $ cat > dune-project <<'EOF'
  > (lang dune 2.0)
  > (using action-plugin 0.1)
  > EOF
  $ cp bin/foo.exe .
  $ mkdir elsewhere
  $ echo first > elsewhere/value
  $ echo root > root.txt
  $ echo child > elsewhere/child.txt
  $ cat > elsewhere/dune <<'EOF'
  > (rule (target input) (action (copy value input)))
  > EOF
  $ cat > dune <<'EOF'
  > (rule (target input) (action (write-file input launch)))
  > (rule
  >  (target cwd-output)
  >  (action
  >   (with-stdout-to cwd-output (dynamic-run ./foo.exe read-in elsewhere input))))
  > (rule
  >  (target listing)
  >  (action (with-stdout-to listing (dynamic-run ./foo.exe list-in elsewhere))))
  > (rule
  >  (target in-flight)
  >  (action (with-stdout-to in-flight (dynamic-run ./foo.exe in-flight))))
  > (rule
  >  (target listing-in-flight)
  >  (action
  >   (with-stdout-to listing-in-flight (dynamic-run ./foo.exe list-in-flight))))
  > (rule
  >  (target parent-output)
  >  (action
  >   (with-stdout-to parent-output
  >    (dynamic-run ./foo.exe read-in elsewhere ../input))))
  > (rule
  >  (target helper-output)
  >  (action (with-stdout-to helper-output (dynamic-run ./foo.exe helper-in))))
  > EOF

A cold read builds the current directory's input, not the launch directory's.

  $ dune build cwd-output > cold.log 2>&1; echo $?
  0
  $ test -f _build/default/input
  [1]

Changing the file actually read invalidates the action, even when both inputs
are already built. These prebuilds are separate targets, not static dependencies
of cwd-output.

  $ dune build input elsewhere/input
  $ dune build cwd-output
  $ cat _build/default/cwd-output
  first
  
  $ echo second > elsewhere/value
  $ dune build elsewhere/input
  $ dune build cwd-output
  $ cat _build/default/cwd-output
  second
  

Directory listings track membership in the directory actually read.

  $ dune build elsewhere/child.txt
  $ dune build listing
  $ cat _build/default/listing
  child.txt
  $ echo added > elsewhere/added.txt
  $ dune build elsewhere/added.txt
  $ dune build listing
  $ cat _build/default/listing
  added.txt
  child.txt

Changing cwd after submitting a request does not redirect the eventual I/O.

  $ dune build in-flight listing-in-flight
  $ cat _build/default/in-flight
  launch
  $ cat _build/default/listing-in-flight
  root.txt

Parent-relative paths should also be interpreted from the current cwd. Helpers
may start in a different directory while sharing the same action ID.

  $ dune build parent-output > parent.log 2>&1; echo $?
  0
  $ cat _build/default/parent-output
  launch
  $ dune build helper-output > helper.log 2>&1; echo $?
  1
  $ cat _build/default/helper-output

The same cold read must work in sandboxes, without prebuilding the input or
assuming the sandbox has the same root as the canonical build tree.

  $ for mode in copy symlink hardlink; do
  >   dune build --sandbox=$mode --build-dir=_build-$mode \
  >     cwd-output > $mode.log 2>&1
  >   echo "$mode: $?"
  > done
  copy: 0
  symlink: 0
  hardlink: 0
  $ cat _build-copy/default/cwd-output
  second
  
  $ cmp _build-copy/default/cwd-output _build-symlink/default/cwd-output
  $ cmp _build-copy/default/cwd-output _build-hardlink/default/cwd-output

The build directory may itself be reached through a symlink. The client's
physical cwd and Dune's spelling of the root still refer to the same tree.

  $ mkdir _build-physical
  $ ln -s _build-physical _build-linked
  $ dune build --build-dir=_build-linked cwd-output > linked.log 2>&1; echo $?
  0
  $ cat _build-linked/default/cwd-output
  second
  

Absolute paths can name generated files. Batched declarations allow ordinary
subprocess I/O without copying whole files into the plugin's heap.

  $ cat >> dune <<'EOF'
  > (rule (target abs-input) (action (write-file abs-input absolute)))
  > (rule
  >  (target abs-output)
  >  (action (with-stdout-to abs-output
  >   (dynamic-run ./foo.exe absolute abs-input))))
  > (rule (target first) (action (write-file first first)))
  > (rule (target second) (action (write-file second second)))
  > (rule
  >  (target batched)
  >  (action (with-stdout-to batched (dynamic-run ./foo.exe batch first second))))
  > EOF
  $ dune build -j 1 abs-output batched
  $ cat _build/default/abs-output
  absolute
  $ cat _build/default/batched; echo
  firstsecond

A batch may mix files, globs, and directories. Paths use the client's cwd when
called, even if it changes while the request is pending.

  $ mkdir -p mixed/glob mixed/directory
  $ cat > mixed/dune <<'EOF'
  > (rule (target one) (action (write-file one file)))
  > EOF
  $ cat > mixed/glob/dune <<'EOF'
  > (rule (target two.txt) (action (write-file two.txt glob)))
  > (rule (target ignored.ml) (action (run false)))
  > EOF
  $ cat > mixed/directory/dune <<'EOF'
  > (rule (target three) (action (write-file three directory)))
  > EOF
  $ cat >> dune <<'EOF'
  > (rule
  >  (target mixed-output)
  >  (action (with-stdout-to mixed-output (dynamic-run ./foo.exe mixed-batch))))
  > EOF
  $ dune build -j 1 mixed-output
  $ cat _build/default/mixed-output; echo
  fileglobdirectory
  $ test ! -e _build/default/mixed/glob/ignored.ml

The promise-returning runner leaves ownership of the event loop with the caller.

  $ cat >> dune <<'EOF'
  > (rule (target some_dependency) (action (write-file some_dependency promise)))
  > (rule
  >  (target promise-output)
  >  (action (with-stdout-to promise-output (dynamic-run ./foo.exe promise))))
  > EOF
  $ dune build -j 1 promise-output
  $ cat _build/default/promise-output
  promise
  returned

Manually deleting Dune-owned artifacts bypasses the workspace cache. This is
not specific to DAP: even building the ordinary producer trusts its record.

  $ rm _build/default/abs-input _build/default/abs-output
  $ dune build abs-input abs-output
  $ test -e _build/default/abs-input
  [1]
  $ test -e _build/default/abs-output
  [1]

Absolute files outside the build tree remain readable. The build directory may
also live outside the source workspace.

  $ external_input=$(mktemp)
  $ echo external > "$external_input"
  $ cat >> dune <<EOF
  > (rule
  >  (target external-output)
  >  (action (with-stdout-to external-output
  >   (dynamic-run ./foo.exe absolute "$external_input"))))
  > EOF
  $ dune build external-output
  $ cat _build/default/external-output
  external
  
  $ external_build=$(mktemp -d)
  $ dune build --build-dir="$external_build" abs-output external-output
  $ cat "$external_build/default/abs-output"
  absolute
  $ cat "$external_build/default/external-output"
  external
  
  $ ln -s "$external_build" _build-external-link
  $ dune build --build-dir=_build-external-link abs-output external-output
  $ cat _build-external-link/default/abs-output
  absolute
  $ cat _build-external-link/default/external-output
  external
  
  $ rm "$external_input"
