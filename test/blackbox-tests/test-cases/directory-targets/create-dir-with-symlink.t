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
