Without a lock directory, pkg is disabled by default
  $ dune pkg enabled
  [1]

  $ make_lockdir

With one that is properly detected, pkg is enabled
  $ test -d dune.lock
  $ dune pkg enabled

The --ignore-lock-dir flag reverts to the initial behaviour as expected
  $ dune pkg enabled --ignore-lock-dir
  [1]
