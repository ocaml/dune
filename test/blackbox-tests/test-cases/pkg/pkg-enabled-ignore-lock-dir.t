Without a lock directory, pkg is disabled by default
  $ dune pkg enabled
  [1]

  $ make_lockdir

With one that is properly detected, pkg is enabled
  $ test -d dune.lock
  $ dune pkg enabled

However the --ignore-lock-dir option raises a bug in the detection mechanism
  $ dune pkg enabled --ignore-lock-dir
^^^^^^^^
This should be [1]
