  $ DUNE_RUNNING=0

  $ start_dune () {
  >  ((dune build "$@" --passive-watch-mode > dune-output 2>&1) || (echo exit $? >> dune-output)) &
  >   DUNE_PID=$!;
  >   DUNE_RUNNING=1;
  > }

  $ timeout="$(command -v timeout || echo gtimeout)"

  $ with_timeout () {
  >   $timeout 2 "$@"
  >   exit_code=$?
  >   if [ "$exit_code" = 124 ]
  >   then
  >     printf "Timed out"
  >   else
  >     return "$exit_code"
  >   fi
  > }

  $ build () {
  >   with_timeout dune rpc build --wait "$@"
  > }

----------------------------------------------------------------------------------
* Compile a simple rule

  $ echo "(lang dune 2.0)" > dune-project

  $ cat > x <<EOF
  > original-contents
  > EOF

  $ cat >dune <<EOF
  > (rule
  >  (target y)
  >  (deps x)
  >  (action (bash "cat x > y")))
  > EOF

  $ start_dune

  $ build y
  Success
  $ cat _build/default/y
  original-contents

  $ echo new-contents > x

  $ build y
  Success
  $ cat _build/default/y
  new-contents

  $ echo new-contents2 > x

  $ build y
  Success
  $ cat _build/default/y
  new-contents2

----------------------------------------------------------------------------------
* File rename

  $ mv x z
  $ build y
  Failure

  $ echo new-contents3 > z

  $ build y
  Failure

  $ mv z x
  $ build y
  Success
  $ cat _build/default/y
  new-contents3

  $ with_timeout dune shutdown
  $ cat dune-output
  waiting for inotify sync
  waited for inotify sync
  Success, waiting for filesystem changes...
  waiting for inotify sync
  waited for inotify sync
  Success, waiting for filesystem changes...
  waiting for inotify sync
  waited for inotify sync
  Success, waiting for filesystem changes...
  waiting for inotify sync
  waited for inotify sync
  File "dune", line 1, characters 0-57:
  1 | (rule
  2 |  (target y)
  3 |  (deps x)
  4 |  (action (bash "cat x > y")))
  Error: No rule found for x
  Had errors, waiting for filesystem changes...
  waiting for inotify sync
  waited for inotify sync
  File "dune", line 1, characters 0-57:
  1 | (rule
  2 |  (target y)
  3 |  (deps x)
  4 |  (action (bash "cat x > y")))
  Error: No rule found for x
  Had errors, waiting for filesystem changes...
  waiting for inotify sync
  waited for inotify sync
  Success, waiting for filesystem changes...

