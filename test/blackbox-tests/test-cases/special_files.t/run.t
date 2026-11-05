  $ mkfifo src/foo
  $ dune build @all
  $ _build/default/bar.exe
  hi!
  $ rm -f src/foo

Check the error message Dune produces when it encounters a broken
symlink:

  $ ln -s broken-link src/foo
  $ dune build @all
  File "src/foo", line 1, characters 0-0:
  Error: File unavailable: src/foo
  Broken symbolic link
  [1]
  $ _build/default/bar.exe
  hi!
  $ rm -f src/foo

A symlink to a FIFO must be rejected before attempting to open it. Keep a
writer ready so that an unexpected read cannot block the test.

  $ mkfifo src/pipe
  $ ln -s pipe src/link
  $ { printf contents > src/pipe; } 2>/dev/null & writer=$!
  $ dune build src/link
  File "src/link", line 1, characters 0-0:
  Error: File unavailable: src/link
  Unexpected file kind "S_FIFO" (named pipe)
  [1]
  $ { kill "$writer"; wait "$writer"; } 2>/dev/null || true
  $ rm src/link src/pipe
