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

We can now run this test, which will record its PID.
  $ dune build @mycram

We can now check if this PID is running. Since it suceeded, it means our
subprocess is still around and has *not* been killed by dune.
  $ kill -0 $(cat $FILE) 2> /dev/null

We manually kill it ourselves by this point.
  $ kill -9 $(cat $FILE)

