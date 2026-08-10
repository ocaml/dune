Test for a project which depends on different versions of the same package depending on the platform.

  $ mkrepo
  $ add_mock_repo_if_needed

Define 2 versions of the package foo that write their version number to a file
during their build so we can validate which version was built.

  $ mkpkg foo 1 <<EOF
  > build: [
  >   ["mkdir" "-p" share "%{lib}%/%{name}%"]
  >   ["touch" "%{lib}%/%{name}%/META"] # needed for dune to recognize this as a library
  >   ["sh" "-c" "echo %{version}% > %{share}%/version"]
  > ]
  > EOF
  $ mkpkg foo 2 <<EOF
  > build: [
  >   ["mkdir" "-p" share "%{lib}%/%{name}%"]
  >   ["touch" "%{lib}%/%{name}%/META"] # needed for dune to recognize this as a library
  >   ["sh" "-c" "echo %{version}% > %{share}%/version"]
  > ]
  > EOF

Define a package bar which conditionally depends on different versions of foo:

  $ make_platform_dependent_bar_package

  $ make_x_depends_bar_project

Linux requires foo.1 while macos requires foo.2. The post-solve invariant
rejects the inconsistent result before a lock directory is written:

  $ DUNE_TRACE=+sat dune pkg lock
  Error: Multi-platform solving selected different versions of the same package
  on different platforms. This is not supported.
  The following packages have version conflicts:
  - foo:version 1 on:- arch = arm64; os = linux
                     - arch = x86_64; os = linux
    version 2 on:- arch = arm64; os = macos
                 - arch = x86_64; os = macos
  [1]

The SAT engine runs once across all requested platforms.

  $ dune trace cat \
  > | jq -s 'include "dune"; [ .[] | satSolveEvents ] | length'
  1

No partial lock directory is written:
  $ test ! -e dune.lock
