A signal received while Dune is running a cram test must not leave the cram
action's sandbox behind. The fiber running the action never resumes after the
signal, so the scheduler deletes the sandboxes registered with it on shutdown.

Run a nested Dune so the cram test can signal its parent without interrupting
this test's own runner. The wrapper exports its PID before replacing itself
with Dune.

  $ mkdir project
  $ cd project
  $ cat > dune-project <<'EOF'
  > (lang dune 3.21)
  > (cram enable)
  > EOF
  $ cat > interrupted.t <<'EOF'
  >   $ mkdir residue
  >   $ echo sandbox-was-not-cleaned > residue/marker
  >   $ kill -INT "$DUNE_UNDER_TEST_PID"
  >   $ sleep 10
  > EOF
  $ cat > run-dune.sh <<'EOF'
  > #!/bin/sh
  > export DUNE_UNDER_TEST_PID=$$
  > exec dune runtest interrupted.t
  > EOF
  $ chmod +x run-dune.sh

  $ ./run-dune.sh >dune.output 2>&1 &
  $ dune_pid=$!
  $ wait "$dune_pid"
  [130]

The nested Dune has exited and its sandbox is gone, including the marker file
the interrupted action wrote into it. Only the markers that keep git and hg
from escaping the sandbox root remain.

  $ find _build/.sandbox -type f -path '*/default/residue/marker' \
  >   | sed -E 's#_build/.sandbox/[0-9a-f]+#_build/.sandbox/<hash>#'
  $ find _build/.sandbox -mindepth 1 -maxdepth 1 ! -name .git ! -name .hg
