The revision store lock file must remain linked while the lock is held. Otherwise a
waiter can acquire the unlinked inode while a new process locks a replacement inode.

Create two independent projects backed by the same local repository. Both Dune
processes below use one fresh revision-store cache.

  $ make_committed_mock_repo_package foo 1.0
  $ repository="git+file://$(pwd)/mock-opam-repository"
  $ for participant in one two; do
  >   mkdir "$participant"
  >   (
  >     cd "$participant"
  >     make_bar_depends_foo_project
  >     add_mock_repo_if_needed "$repository"
  >   )
  > done

A fake git blocks the first process in `git init` and the second process in
`git rev-parse`. These are both called from `Rev_store.load_or_create` while
Dune holds the revision-store lock.

  $ real_git=$(command -v git)
  $ timeout=$(command -v timeout)
  $ mkdir fake-bin sync
  $ mkfifo sync/release-one sync/release-two
  $ cat > fake-bin/git <<'EOF'
  > #!/bin/sh
  > set -eu
  > case "${PARTICIPANT-}:$1" in
  >   one:init)
  >     : > "$SYNC/one-entered"
  >     "$TIMEOUT" 30 sh -c 'IFS= read -r _ < "$1"' sh "$SYNC/release-one"
  >     ;;
  >   two:rev-parse)
  >     : > "$SYNC/two-entered"
  >     "$TIMEOUT" 30 sh -c 'IFS= read -r _ < "$1"' sh "$SYNC/release-two"
  >     ;;
  > esac
  > exec "$REAL_GIT" "$@"
  > EOF
  $ chmod +x fake-bin/git
  $ SYNC=$(pwd)/sync
  $ CACHE=$(pwd)/cache
  $ REAL_GIT=$real_git
  $ TIMEOUT=$timeout
  $ PATH=$(pwd)/fake-bin:$PATH
  $ export SYNC CACHE REAL_GIT TIMEOUT PATH

  $ release_fifo_waiters() {
  >   for fifo in "$SYNC/release-one" "$SYNC/release-two"; do
  >     if [ -p "$fifo" ]; then
  >       "$TIMEOUT" 1 sh -c 'printf "release\n" > "$1"' sh "$fifo" >/dev/null 2>&1 || true
  >     fi
  >   done
  > }
  $ cleanup() {
  >   release_fifo_waiters
  >   for pid in ${one_pid-} ${two_pid-}; do
  >     if [ -n "$pid" ] && kill -0 "$pid" 2>/dev/null; then
  >       kill -KILL "$pid" 2>/dev/null || true
  >     fi
  >   done
  >   release_fifo_waiters
  >   for pid in ${one_pid-} ${two_pid-}; do
  >     if [ -n "$pid" ]; then wait "$pid" 2>/dev/null || true; fi
  >   done
  > }
  $ trap cleanup EXIT HUP INT TERM

  $ wait_for_dune_process() {
  >   pid=$1
  >   output=$2
  >   if wait_for_pid_to_exit_with_timeout "$pid" 500; then
  >     wait "$pid"
  >   else
  >     cat "$output"
  >     release_fifo_waiters
  >     kill -KILL "$pid" 2>/dev/null || true
  >     release_fifo_waiters
  >     wait "$pid" 2>/dev/null || true
  >     return 124
  >   fi
  > }

This helper is a bounded readiness barrier: it returns only once the second
Dune process has opened the same lock inode as the first process.

  $ cat > wait-for-open-lock <<'EOF'
  > #!/bin/sh
  > set -eu
  > pid=$1
  > lock=$2
  > while kill -0 "$pid" 2>/dev/null; do
  >   for fd in /proc/"$pid"/fd/*; do
  >     if [ "$fd" -ef "$lock" ]; then
  >       exit 0
  >     fi
  >   done
  >   sleep 0.01
  > done
  > exit 1
  > EOF
  $ chmod +x wait-for-open-lock

Hold the first process in initialization, then start the second and wait until
it has opened the original lock inode before releasing the first.

  $ cd one
  $ PARTICIPANT=one XDG_CACHE_HOME="$CACHE" dune pkg lock > ../one.out 2>&1 &
  $ one_pid=$!
  $ cd ..
  $ with_timeout_quiet dune_cmd wait-for-file-to-appear "$SYNC/one-entered"
  $ cd two
  $ PARTICIPANT=two XDG_CACHE_HOME="$CACHE" dune pkg lock > ../two.out 2>&1 &
  $ two_pid=$!
  $ cd ..
  $ with_timeout_quiet ./wait-for-open-lock "$two_pid" "$CACHE/dune/rev-store.lock"
  $ "$timeout" 2 sh -c 'printf "release\n" > "$1"' sh "$SYNC/release-one"
  $ with_timeout_quiet dune_cmd wait-for-file-to-appear "$SYNC/two-entered"
  $ wait_for_dune_process "$one_pid" one.out

The second Dune process now holds the original inode inside
`Rev_store.load_or_create`. A third process opening the canonical lock path
must contend with that lock rather than acquire a replacement inode.

  $ if flock -E 75 -n "$CACHE/dune/rev-store.lock" true; then
  >   echo "third process entered the revision-store critical section"
  > else
  >   status=$?
  >   if [ "$status" -eq 75 ]; then
  >     echo "third process was blocked by the revision-store lock"
  >   else
  >     echo "flock probe failed with status $status"
  >     exit "$status"
  >   fi
  > fi
  third process was blocked by the revision-store lock

Release the second process and bound the wait for it to exit.

  $ "$timeout" 2 sh -c 'printf "release\n" > "$1"' sh "$SYNC/release-two"
  $ wait_for_dune_process "$two_pid" two.out
  $ trap - EXIT HUP INT TERM
