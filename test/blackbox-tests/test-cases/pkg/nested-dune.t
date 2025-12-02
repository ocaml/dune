Show the behavior if a dune is captured from PATH. Dune calling a nested dune
should call itself, not some other Dune captured from the environment.

  $ . ./helpers.sh

Create a fake dune that will get added to the PATH. This dune will, for
simplicity, always fail.

  $ mkdir .fakebin
  $ cat > .fakebin/dune <<EOF
  > #!/usr/bin/env sh
  > echo "Wrong Dune (captured from the environment), failing" >&2
  > exit 1
  > EOF
  $ chmod +x .fakebin/dune

Our test package just calls dune and expects a working dune.

  $ make_lockdir
  $ make_lockpkg test <<EOF
  > (version 0.0.1)
  > (build (run dune build @missing))
  > EOF

Run `build_pkg` with the dune under test, but expand the path with the captured
dune that doesn't work.

  $ dune=$(which dune)
  $ PATH=$(pwd)/.fakebin:$PATH build_pkg test
  File "dune.lock/test.pkg", line 2, characters 12-16:
  2 | (build (run dune build @missing))
                  ^^^^
  Error: Logs for package test
  Wrong Dune (captured from the environment), failing
  
  [1]
