Modify an input file during the build so that dune interrupts the
build

  $ make_dune_project 2.0

  $ cat > x <<EOF
  > original-contents
  > EOF

  $ cat >dune <<'EOF'
  > (rule
  >  (target y)
  >  (deps x)
  >  (action (bash "\> if [[ "$(cat x)" == unstable ]]; then
  >                "\>   touch ../../go-ahead
  >                "\>   sleep 1000
  >                "\>   exit 1
  >                "\> else
  >                "\>   cat x > y
  >                "\> fi
  > )))
  > EOF

The rule above makes the test hang if it sees an "unstable" state of
the file.  This makes it easy to make sure that the dune won't finish
before we're able to cancel the build.

  $ start_dune

  $ build y
  Success
  $ cat _build/default/y
  original-contents

  $ echo unstable > x

  $ (dune_cmd wait-for-file-to-appear go-ahead; echo new-contents > x) & build y
  Success

  $ cat _build/default/y
  new-contents

  $ stop_dune_quiet
