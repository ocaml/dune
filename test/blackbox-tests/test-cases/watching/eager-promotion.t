This test shows that the eager watch server doesn't register promotions at all

  $ . ./helpers.sh

  $ echo '(lang dune 3.20)' > dune-project

  $ cat > dune << EOF
  > (rule 
  >  (alias a)
  >  (action
  >   (progn
  >    (write-file a.output "processed A")
  >    (diff a a.output))))
  > EOF

  $ echo "raw A" > a

  $ dune build -w &
  File "a", line 1, characters 0-0:
  Error: Files _build/default/a and _build/default/a.output differ.
  Had 1 error, waiting for filesystem changes...
  File "a", line 1, characters 0-0:
  Error: Files _build/default/a and _build/default/a.output differ.
  Had 1 error, waiting for filesystem changes...
  Warning: Nothing to promote for a.
  $ dune rpc ping --wait
  Server appears to be responding normally
Added safety measure against racy behaviour.
  $ sleep 1

Now A should be in the promotion database
  $ dune build @a
  File "a", line 1, characters 0-0:
  Error: Files _build/default/a and _build/default/a.output differ.
  Error: Build failed with 1 error.
  [1]

This should be raw, as we haven't promoted yet
  $ cat a
  raw A

The promotion database should exist!
  $ find _build/.to-promote
  find: '_build/.to-promote': No such file or directory
  [1]

This should be a success
  $ dune promote a
  Success

This should be processed
  $ cat a
  raw A

  $ dune shutdown
  $ wait
