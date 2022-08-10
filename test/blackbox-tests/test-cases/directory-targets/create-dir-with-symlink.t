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
  File "dune", line 1, characters 0-156:
  1 | (rule
  2 |  (targets (dir symlinked))
  3 |  ;; not exactly correcty, but it's just a test
  4 |  (deps bar/foo (sandbox always))
  5 |  (action (system "ln -s ./bar symlinked")))
  Error: Rule produced a file with unrecognised kind "S_LNK"
  [1]

  $ {
  > if [ -e _build/default/symlinked ]
  > then
  >   echo symlink exists
  > else
  >   echo symlink does not exist
  > fi
  > }
  symlink does not exist
