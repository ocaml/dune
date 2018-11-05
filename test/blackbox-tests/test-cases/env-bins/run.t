Basic test that we can use private binaries as public ones
  $ dune build --root private-bin-import
  Entering directory 'private-bin-import'
          priv alias using-priv/runtest
  Executing priv as priv.exe
          priv alias using-priv/runtest
  Executing priv as priv.exe

Override public binary in env
  $ dune build --root override-bins
  Entering directory 'override-bins'
          priv alias test/runtest
  private binary
           foo alias default
  public binary
