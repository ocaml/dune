  $ . ./opam-var-helpers.sh

Here we test the translation and implementation of global opam variables. OS specific
variables can be found in `opam-var-os.t`.

  $ mkrepo
  > mkpkg testpkg << EOF
  > build: [
  >   [ "echo" jobs ]
  >   [ "echo" make ]
  >   [ "echo" user ] 
  >   [ "echo" group ]
  > ]
  > EOF
  > solve testpkg
  Solution for dune.lock:
  testpkg.0.0.1
  
  $ cat dune.lock/testpkg.pkg
  (version 0.0.1)
  
  (build
   (progn
    (run echo %{jobs})
    (run echo %{make})
    (run echo %{user})
    (run echo %{group})))

The implementation of %{user} uses Unix.getlogin which doesn't work in our Linux CI job.
Therefore we modify the lockfile here to remove that from the opam file:

  $ mkpkg testpkg << EOF
  > build: [
  >   [ "echo" jobs ]
  >   [ "echo" make ]
  >   [ "echo" group ]
  > ]
  > EOF
  > solve testpkg
  Solution for dune.lock:
  testpkg.0.0.1
  
If this is fixed, we should use $(whoami) to compare the output.

The value for "jobs" should always be 1.

  $ MAKE="$(which make)"
  > GROUP="$(id -gn)"
  > build_pkg testpkg 2>&1 \
  > | sed "s/$GROUP/GROUP/g" | sed "s#$MAKE#MAKE#g"
  File "dune.lock/testpkg.pkg", line 5, characters 12-19:
  5 |   (run echo %{jobs})
                  ^^^^^^^
  Error: Unknown variable %{jobs}
