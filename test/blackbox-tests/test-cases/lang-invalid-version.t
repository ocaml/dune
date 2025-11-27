Test invalid version numbers in dune-project files. We want to make sure that
such situations provide a clear error.

  $ test_invalid_version() {
  >   cat > dune-project <<EOF
  > (lang dune $1)
  > EOF
  >   dune build 2>&1 | grep "Internal error"
  > }

Invalid version number:

  $ test_invalid_version "Ali"
  [1]

Test with various non-ASCII characters:

  $ test_invalid_version "è"
  Internal error, please report upstream including the contents of _build/log.

  $ test_invalid_version "π3.14"
  Internal error, please report upstream including the contents of _build/log.

  $ test_invalid_version "α"
  Internal error, please report upstream including the contents of _build/log.

  $ test_invalid_version "😀"
  Internal error, please report upstream including the contents of _build/log.

  $ test_invalid_version "中3.16文"
  Internal error, please report upstream including the contents of _build/log.
