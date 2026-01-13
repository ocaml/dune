Repro `dune exec --watch` crash with pkg management

  $ mkdir external_sources
  $ cat >external_sources/dune-project <<EOF
  > (lang dune 3.11)
  > (package (name mypkg))
  > EOF
  $ cat >external_sources/dune <<EOF
  > (library
  >  (public_name mypkg.lib)
  >  (name test_lib))
  > EOF
  $ cat >external_sources/test_lib.ml <<EOF
  > let x = "hello"
  > EOF

Now we set up a lock file with this package and then attempt to use it:

  $ cat >dune-project <<EOF
  > (lang dune 3.11)
  > EOF

  $ make_lockdir
  $ make_lockpkg mypkg <<EOF
  > (version 0.0.1)
  > (source (copy $PWD/external_sources))
  > (build
  >   (run dune build --release --promote-install-file=true . @install))
  > EOF

  $ cat >dune <<EOF
  > (dirs (:standard \ external_sources))
  > (executable
  >  (name x)
  >  (libraries mypkg.lib))
  > EOF

  $ cat >x.ml <<EOF
  > let () = print_endline Test_lib.x
  > EOF

  $ dune exec -w ./x.exe > output.log 2>&1 &
  $ PID=$!

  $ TIME_WAITED=0
  $ MAX_WAIT_TIME=20
  $ SLEEP_INTERVAL=1
  $ while [ $(cat output.log | wc -l) -lt 2 ] && [ "$TIME_WAITED" -lt "$MAX_WAIT_TIME" ]; do
  >   sleep 0.1
  >   TIME_WAITED=$((TIME_WAITED + SLEEP_INTERVAL))
  > done

  $ cat output.log | sort
  Success, waiting for filesystem changes...
  hello
  $ kill $PID
  $ wait $PID
