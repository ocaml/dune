Test invalid version numbers in dune-project files. We want to make sure that
such situations provide a clear error.

  $ test_invalid_version() {
  >   cat > dune-project <<EOF
  > (lang dune $1)
  > EOF
  >   dune build 2>&1 | grep "Invalid version string"
  > }

Invalid version number:

  $ test_invalid_version "Ali"
  [1]

Test with various non-ASCII characters:

  $ test_invalid_version "è"
  Error: Invalid version string

  $ test_invalid_version "π3.14"
  Error: Invalid version string

  $ test_invalid_version "α"
  Error: Invalid version string

  $ test_invalid_version "😀"
  Error: Invalid version string

  $ test_invalid_version "中3.16文"
  Error: Invalid version string
