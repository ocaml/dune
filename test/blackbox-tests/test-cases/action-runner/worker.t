Connecting without server should fail

  $ DUNE_RPC="unix:path=foo" dune internal action-runner start foobar 2>&1 | sed -n '/^Error/,/backtrace:/p'
  Error: failed to connect to RPC server unix://foo
  Unix.Unix_error(Unix.ENOENT, "connect", "")
  backtrace:
