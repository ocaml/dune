Modify an input file during the build so that Dune interrupts the build once.

Bad rule! You are not supposed to modify the source tree. No ice-cream for you!

  $ mkdir test
  $ cd test

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
  I'm seeing: ./old-source.txt
  I'm seeing: ./new-source.txt ./old-source.txt

  $ stop_dune_quiet
  $ cd ..

Touching an input must not restart a running build, but a content edit must
cancel it and rebuild using the new contents.

  $ mkdir source-copy-change-cutoff
  $ cd source-copy-change-cutoff
  $ make_dune_project 3.0
  $ setup_xdg_runtime_dir
  $ export DUNE_TRACE=build,rpc
  $ started="$PWD/../source-copy-started"
  $ release="$PWD/../source-copy-release"
  $ mkfifo "$release"
  $ printf one > data
  $ cat > dune <<EOF
  > (alias (name idle))
  > (rule
  >  (deps data (sandbox none))
  >  (target result)
  >  (action
  >   (bash "\| if [ \"\$(cat data)\" = one ]; then
  >          "\|   touch '$started'
  >          "\|   read _ < '$release'
  >          "\| fi
  >          "\| cat data > result
  > )))
  > EOF
  $ start_dune @idle
  $ dune rpc build --wait result > .#build-output 2>&1 &
  $ BUILD_PID=$!
  $ with_timeout dune_cmd wait-for-file-to-appear "$started"
  $ restarts_before_touch=$(dune trace cat | jq -s '
  >   [.[] | select(.cat == "build" and .name == "build-start"
  >                and .args.restart == true)] | length')
  $ touch data
  $ with_timeout dune rpc flush-file-watcher --wait
  $ restarts_after_touch=$(dune trace cat | jq -s '
  >   [.[] | select(.cat == "build" and .name == "build-start"
  >                and .args.restart == true)] | length')
  $ test "$restarts_after_touch" = "$restarts_before_touch"
  $ printf two > data
  $ with_timeout dune rpc flush-file-watcher --wait
  $ if wait_for_pid_to_exit_with_timeout "$BUILD_PID" 200; then
  >   wait "$BUILD_PID"
  > else
  >   echo "build did not restart"
  >   printf '.\n' > "$release"
  >   wait "$BUILD_PID"
  > fi
  $ cat .#build-output
  Success
  $ cat _build/default/result
  two
  $ dune trace cat | jq -s --argjson before "$restarts_after_touch" '
  >   [.[] | select(.cat == "build" and .name == "build-start"
  >                and .args.restart == true)] | length > $before'
  true
  $ stop_dune_quiet
  $ unset DUNE_TRACE
