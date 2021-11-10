Modify an input file during the build so that dune interrupts the
build

  $ . ./helpers.sh

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

Below we hide the Restart outcome because the reason is non-deterministic. We
can see "Restart (. changed)" or "Restart (. changed, and 1 more change)"
depending on how quickly Dune reacts to the first change.

  $ ((x=0;
  >   while [ ! -f _build/build-finished ];
  >     do
  >       x=$((x+1));
  >       echo "$x-unstable" > z;
  >       mv z x;
  >       sleep 0.01;
  >     done
  >  ) & build y; touch _build/build-finished; wait
  > ) | grep Restart | dune_cmd count-lines
  1

  $ echo new-contents2 > x

  $ build y
  Success
  $ cat _build/default/y
  new-contents2

  $ stop_dune
  Success, waiting for filesystem changes...
  Success, waiting for filesystem changes...
