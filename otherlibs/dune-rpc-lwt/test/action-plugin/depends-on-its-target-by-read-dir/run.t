  $ cat > dune-project << EOF
  > (lang dune 2.0)
  > (using action-plugin 0.1)
  > EOF

  $ cat > dune << EOF
  > (rule
  >  (target some_file)
  >  (action
  >   (dynamic-run ./foo.exe)))
  > EOF

  $ cp ./bin/foo.exe ./

Ignore signal-handler output when terminating a deadlocked build, but preserve
all diagnostics when the build exits without timing out.

  $ build_with_timeout() {
  >   output=$(mktemp)
  >   status=0
  >   timeout 3 dune build "$@" >"$output" 2>&1 || status=$?
  >   if [ "$status" -ne 124 ]; then cat "$output"; fi
  >   rm "$output"
  >   return "$status"
  > }

Reading a directory containing the action's target currently deadlocks.

  $ build_with_timeout some_file
  [124]
