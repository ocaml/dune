A signal received while Dune is running a cram test leaves the cram action's
sandbox behind.

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

The nested Dune has exited, but an ordinary file is still present in its
sandbox. No socket, FIFO, or other special filesystem entry is needed to
reproduce the leak.

  $ marker=$(find _build/.sandbox -type f -path '*/default/residue/marker')
  $ cat "$marker"
  sandbox-was-not-cleaned
  $ echo "$marker" | sed -E 's#_build/.sandbox/[0-9a-f]+#_build/.sandbox/<hash>#'
  _build/.sandbox/<hash>/default/residue/marker
