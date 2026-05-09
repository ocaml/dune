Modify an input file during the build so that Dune interrupts the build once.

Bad rule! You are not supposed to modify the source tree. No ice-cream for you!

  $ mkdir test
  $ cd test
  $ setup_xdg_runtime_dir

  $ echo '(lang dune 3.0)' > dune-project
  $ cat > dune <<EOF
  > (rule
  >  (deps (glob_files *.txt) (sandbox none))
  >  (alias default)
  >  (action (system "\| echo "I'm seeing: %{deps}" >> ../../../output
  >                  "\| touch ../../new-source.txt
  > )))
  > EOF

  $ touch old-source.txt
  $ start_dune

  $ build .
  Success

We can see in the output below that the rule ran exactly twice. Note that the
file watcher generates the next touch event for new-source.txt but it
is ignored because the contents of new-source.txt is the same,
i.e. the empty file.

  $ cat ../output
  I'm seeing: old-source.txt
  I'm seeing: new-source.txt old-source.txt

  $ stop_dune
  Success, waiting for filesystem changes...

  $ cd ..

Touching a source file while a build is running does not restart the build.
A content edit should restart the running build.

  $ mkdir source-copy-change-cutoff
  $ cd source-copy-change-cutoff
  $ export DUNE_TRACE=build,rpc
  $ setup_xdg_runtime_dir
  $ started="$PWD/../started"
  $ release="$PWD/../release"
  $ rm -f "$started" "$release"

  $ echo '(lang dune 3.0)' > dune-project
  $ printf one > data
  $ cat > dune <<EOF
  > (rule
  >  (deps data)
  >  (target result)
  >  (action
  >   (bash "\| if [ \"\$(cat data)\" = one ]; then
  >          "\|   touch '$started'
  >          "\|   while [ ! -f '$release' ]; do sleep 0.01; done
  >          "\| fi
  >          "\| cat data > result
  > )))
  > EOF

  $ start_dune
  $ build result > build.out 2>&1 &
  $ BUILD_PID=$!
  $ with_timeout dune_cmd wait-for-file-to-appear "$started"

  $ restarts_before_touch=$(dune trace cat | jq -s '
  > [ .[] | select(.cat == "build"
  >                and .name == "build-start"
  >                and .args.restart == true) ]
  > | length')
  $ touch data
  $ with_timeout dune rpc flush-file-watcher --wait
  $ restarts_after_touch=$(dune trace cat | jq -s '
  > [ .[] | select(.cat == "build"
  >                and .name == "build-start"
  >                and .args.restart == true) ]
  > | length')
  $ [ "$restarts_after_touch" = "$restarts_before_touch" ] && echo true
  true

  $ printf two > data
  $ with_timeout dune rpc flush-file-watcher --wait
  $ if wait_for_pid_to_exit_with_timeout "$BUILD_PID" 200; then
  >   wait "$BUILD_PID"
  > else
  >   echo "build did not restart"
  >   cat build.out
  >   touch "$release"
  >   wait "$BUILD_PID"
  > fi
  $ cat build.out
  Success
  $ cat _build/default/result
  two
  $ dune trace cat | jq -s --argjson restarts_after_touch "$restarts_after_touch" '
  > [ .[] | select(.cat == "build"
  >                and .name == "build-start"
  >                and .args.restart == true) ] as $restarts
  > | $restarts[$restarts_after_touch:] as $new_restarts
  > | ($new_restarts | length >= 1)
  >   and ([ $new_restarts[] | .args.files[]? ] | index("data") != null)'
  true

  $ stop_dune > /dev/null
