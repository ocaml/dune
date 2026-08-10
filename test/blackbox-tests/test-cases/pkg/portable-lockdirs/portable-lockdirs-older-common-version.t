When the solver selects different versions of the same package for different
platforms, lock directory generation fails rather than writing an inconsistent
result.

  $ mkrepo
  $ add_mock_repo_if_needed

Define 2 versions of the package foo that write their version number to a file
during their build so we can validate which version was built. The newer
version is only available on linux:
  $ mkpkg foo 1 <<EOF
  > build: [
  >   ["mkdir" "-p" share "%{lib}%/%{name}%"]
  >   ["touch" "%{lib}%/%{name}%/META"] # needed for dune to recognize this as a library
  >   ["sh" "-c" "echo %{version}% > %{share}%/version"]
  > ]
  > EOF
  $ mkpkg foo 2 <<EOF
  > available: os = "linux"
  > build: [
  >   ["mkdir" "-p" share "%{lib}%/%{name}%"]
  >   ["touch" "%{lib}%/%{name}%/META"] # needed for dune to recognize this as a library
  >   ["sh" "-c" "echo %{version}% > %{share}%/version"]
  > ]
  > EOF

Define a package bar which depends on foo without a version constraint:
  $ mkpkg bar <<EOF
  > build: [
  >   ["mkdir" "-p" share "%{lib}%/%{name}%"]
  >   ["touch" "%{lib}%/%{name}%/META"] # needed for dune to recognize this as a library
  > ]
  > depends: [ "foo" ]
  > EOF

  $ make_x_depends_bar_project

Linux prefers foo.2 while macos can only install foo.1. Without a
cross-platform version constraint, the solve selects both versions and the
post-solve invariant rejects the result:
  $ dune pkg lock
  Error: Multi-platform solving selected different versions of the same package
  on different platforms. This is not supported.
  The following packages have version conflicts:
  - foo:version 1 on:- arch = arm64; os = macos
                     - arch = x86_64; os = macos
    version 2 on:- arch = arm64; os = linux
                 - arch = x86_64; os = linux
  [1]

No partial lock directory is written:
  $ test ! -e dune.lock
