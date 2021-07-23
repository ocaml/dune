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
* Modify an input file during the build so that dune interrupts the build

  $ echo "(lang dune 2.0)" > dune-project

  $ cat > x <<EOF
  > original-contents
  > EOF

  $ cat >dune <<'EOF'
  > (rule
  >  (target y)
  >  (deps x)
  >  (action (bash "if [[ \"$(cat x)\" == *-unstable ]]; then sleep 1000; exit 1; else cat x > y; fi")))
  > EOF

The rule above makes the test hang if it sees an "unstable" state of the file.
This makes it easy to make sure that the dune won't finish before we're able to cancel the build.

  $ start_dune

  $ build y
  Success
  $ cat _build/default/y
  original-contents

  $ echo 0-unstable > x

  $ ((x=0; while [ ! -f _build/build-finished ]; do x=$((x+1)); echo "$x-unstable" > z; mv z x; sleep 0.01; done) & build y; touch _build/build-finished; wait)
  Failure

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
  Had errors, killing current build...
  waiting for inotify sync
  waited for inotify sync
  Success, waiting for filesystem changes...

