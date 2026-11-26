Test creating directory targets by symlinking:

  $ make_directory_targets_project 3.3

  $ cat >dune <<EOF
  > (rule
  >  (targets (dir symlinked))
  >  ;; not exactly correctly, but it's just a test
  >  (deps bar/foo (sandbox always))
  >  (action (system "ln -s ./bar symlinked")))
  > EOF

  $ mkdir bar && touch bar/foo

  $ dune build ./symlinked

  $ {
  > path=_build/default/symlinked
  > if [ -e $path ]
  > then
  >   printf "symlink exists and points to: %s" "$(readlink $path)"
  > else
  >   echo symlink does not exist
  > fi
  > }
  symlink exists and points to: ./bar

Retarget a symlink inside a generated directory without changing the bytes
or executable bit of its referent. An unsandboxed consumer can observe the
link destination directly.

  $ cat > dune <<'EOF'
  > (rule
  >  (target (dir tree))
  >  (deps selection)
  >  (action
  >   (system "\| mkdir tree
  >           "\| echo same > tree/first
  >           "\| cp tree/first tree/second
  >           "\| ln -s $(cat selection) tree/selected
  >   )))
  > (rule
  >  (target destination)
  >  (deps tree (sandbox none))
  >  (action (with-stdout-to destination (run readlink tree/selected))))
  > EOF
  $ echo first > selection
  $ dune build --cache=disabled destination
  $ cat _build/default/destination
  first

CR: The digest records the referent's contents and executable bit but not
which path the symlink names, so the consumer does not run again.

  $ dune_cmd wait-for-fs-clock-to-advance
  $ echo second > selection
  $ dune build --cache=disabled destination
  $ readlink _build/default/tree/selected
  second
  $ cat _build/default/destination
  first
