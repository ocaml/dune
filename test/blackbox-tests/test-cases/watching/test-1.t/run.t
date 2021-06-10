  $ DUNE_RUNNING=0

  $ start_dune () {
  >  ((dune build "$@" --watch=passive > dune-output 2>&1) || (echo exit $? >> dune-output)) &
  >   DUNE_PID=$!;
  >   DUNE_RUNNING=1;
  > }

  $ with_timeout () {
  >   timeout 2 "$@"
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
