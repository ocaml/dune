Characterize automatic concurrency detection after one failed spawn.
Inject EAGAIN once so the retrying implementation can still finish normally,
without a timeout or resource exhaustion.

  $ make_dune_project 3.0
  $ mkdir probes
  $ cat > probes/nproc <<'EOF'
  > #!/bin/sh
  > echo nproc >> .#probes-called
  > echo 2
  > EOF
  $ cat > probes/getconf <<'EOF'
  > #!/bin/sh
  > echo getconf >> .#probes-called
  > echo 1
  > EOF
  $ chmod +x probes/nproc probes/getconf

First observe the spawn syscall: libc may implement vfork using clone.
Without injection the first probe is nproc.

  $ PATH="$PWD/probes:$PATH" strace -f -qq -o .#detect -e trace=process \
  >   dune build -j auto --display=quiet
  $ cat .#probes-called
  nproc
  $ spawn=$(awk '
  >   /(vfork|clone|clone3)\(/ {
  >     sub(/\(.*/, "")
  >     sub(/^.* /, "")
  >     print
  >     exit
  >   }' .#detect)
  $ rm .#probes-called
  $ PATH="$PWD/probes:$PATH" strace -f -qq -o .#syscalls \
  >   -e trace="$spawn" -e inject="$spawn":error=EAGAIN:when=1 \
  >   dune build -j auto --display=quiet
  $ grep -c '(INJECTED)' .#syscalls
  1

Observe which probe runs after the failed spawn.

  $ cat .#probes-called
  getconf

A fixed concurrency setting must not run either detection command.

  $ rm .#probes-called
  $ PATH="$PWD/probes:$PATH" dune build -j 1
  $ test ! -e .#probes-called
