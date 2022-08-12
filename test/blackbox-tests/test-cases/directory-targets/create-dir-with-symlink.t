Test creating directory targets by symlinking:

  $ cat > dune-project <<EOF
  > (lang dune 3.3)
  > (using directory-targets 0.1)
  > EOF

  $ cat >dune <<EOF
  > (rule
  >  (targets (dir symlinked))
  >  ;; not exactly correcty, but it's just a test
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
