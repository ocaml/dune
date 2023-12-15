Make sure we can run exes from the user's PATH variable.

  $ . ./helpers.sh

Create a directory containing a shell script and add the directory to PATH.
  $ mkdir bin
  $ cat > bin/hello <<EOF
  > #!/bin/sh
  > echo "Hello, World!"
  > EOF
  $ chmod a+x bin/hello
  $ export PATH=$PATH:$PWD/bin

Create a lockdir with a lockfile that runs the shell script in a build command.
  $ make_lockdir
  $ cat >dune.lock/test.pkg <<'EOF'
  > (version 0.0.1)
  > (build (run hello))
  > EOF

The build command is run from an environment including the custom PATH variable.
  $ build_pkg test
  Hello, World!
