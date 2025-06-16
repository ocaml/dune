Testing the termination of subprocesses in cram tests. We first create a dune
project with a single cram test.

  $ cat > dune-project <<EOF
  > (lang dune 3.19)
  > EOF

We create a file for tracking the PID of a subprocess.

  $ cat > pid.txt
  $ export FILE=$(realpath ./pid.txt)

We create a cram test that spawns a subprocess and records its PID in the file
we gave before.

  $ cat > mycram.t <<EOF
  >   $ sleep 5 &
  >   $ echo \$! > $FILE
  > EOF

We can now run this test, which will record its PID in the file.

  $ dune build @mycram

The test finished successfully, now we make sure that the PID was correctly
terminated.

  $ ps -p $(cat $FILE) > /dev/null || echo "Process terminated"
  Process terminated
