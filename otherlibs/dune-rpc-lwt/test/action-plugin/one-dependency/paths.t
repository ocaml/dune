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
  0
  $ cat _build/default/helper-output
  second
  

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
  
