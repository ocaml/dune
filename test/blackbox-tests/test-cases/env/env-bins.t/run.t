Basic test that we can use private binaries as public ones
  $ dune build --root private-bin-import
  Entering directory 'private-bin-import'
  Executing priv as priv
  PATH:
  	$TESTCASE_ROOT/private-bin-import/_build/default/using-priv/.bin
  	$TESTCASE_ROOT/private-bin-import/_build/install/default/bin
  Executing priv as priv-renamed
  PATH:
  	$TESTCASE_ROOT/private-bin-import/_build/default/using-priv/.bin
  	$TESTCASE_ROOT/private-bin-import/_build/install/default/bin

Override public binary in env
  $ dune build --root override-bins
  Entering directory 'override-bins'
  private binary
  public binary

Nest env binaries
  $ dune build --root nested-env
  Entering directory 'nested-env'
  Executing priv as priv
  PATH:
  	$TESTCASE_ROOT/nested-env/_build/default/using-priv/nested/.bin
  	$TESTCASE_ROOT/nested-env/_build/default/using-priv/.bin
  	$TESTCASE_ROOT/nested-env/_build/install/default/bin
  Executing priv as priv-renamed
  PATH:
  	$TESTCASE_ROOT/nested-env/_build/default/using-priv/nested/.bin
  	$TESTCASE_ROOT/nested-env/_build/default/using-priv/.bin
  	$TESTCASE_ROOT/nested-env/_build/install/default/bin
  Executing priv as priv-renamed-nested
  PATH:
  	$TESTCASE_ROOT/nested-env/_build/default/using-priv/nested/.bin
  	$TESTCASE_ROOT/nested-env/_build/default/using-priv/.bin
  	$TESTCASE_ROOT/nested-env/_build/install/default/bin
