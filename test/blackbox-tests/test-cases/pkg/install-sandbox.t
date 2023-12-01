We make sure that the package rules are sandboxed correctly.

This is currently not the case, as discovered in #9013.

  $ . ./helpers.sh
  $ mkrepo
  $ mkpkg foo << EOF
  > install: [
  >  [ "mkdir" "-p" "%{lib}%/foo" ]
  >  [ "touch" "%{lib}%/foo/bar" ]
  > ]
  > EOF

  $ solve foo 
  Solution for dune.lock:
  - foo.0.0.1

The %{lib} variable is not expanded correctly for the sandbox, causing the
target of the sandboxed action to be created before the sandbox can.

  $ build_pkg foo
  Error: Target _build/_private/default/.pkg/foo/target of kind "directory"
  already exists in the build directory
  -> required by _build/_private/default/.pkg/foo/target/cookie
  Hint: delete this file manually or check the permissions of the parent
  directory of this file
  [1]
