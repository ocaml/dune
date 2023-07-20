Make sure we can run exes from the user's PATH variable.

Create a directory containing a shell script and add the directory to PATH.
  $ mkdir bin
  $ cat > bin/hello <<EOF
  > #!/bin/sh
  > echo "Hello, World!"
  > EOF
  $ chmod a+x bin/hello
  $ PATH=$PATH:$PWD/bin

Create a lockdir with a lockfile that runs the shell script in a build command.
  $ mkdir dune.lock
  $ cat >dune.lock/lock.dune <<EOF
  > (lang package 0.1)
  > EOF
  $ cat >dune.lock/test.pkg <<'EOF'
  > (build (system hello))
  > EOF

The build command is run from an environment including the custom PATH variable.
  $ dune build .pkg/test/target/
  Hello, World!
