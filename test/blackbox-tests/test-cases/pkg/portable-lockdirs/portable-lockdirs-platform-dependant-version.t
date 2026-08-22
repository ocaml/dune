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

Linux requires foo.1 while macos requires foo.2. The cross-platform version
constraint makes the requested platform set unsatisfiable:

  $ DUNE_TRACE=+sat dune pkg lock
  Error:
  Unable to solve dependencies while generating lock directory: dune.lock
  
  The dependency solver failed to find a solution for the requested platforms:
  - arch = x86_64; os = linux
  - arch = arm64; os = linux
  - arch = x86_64; os = macos
  - arch = arm64; os = macos
  ...with this error:
  Couldn't solve the package dependency formula.
  Selected candidates: bar.0.0.1 foo.1 x.dev
  - foo -> (problem) on arch = arm64; os = macos
      bar 0.0.1 requires = 2
      Rejected candidates:
        foo.2: Version differs from foo.1 selected on arch = x86_64; os = linux
        foo.1: Incompatible with restriction: = 2
  - foo -> (problem) on arch = x86_64; os = macos
      bar 0.0.1 requires = 2
      Rejected candidates:
        foo.2: Version differs from foo.1 selected on arch = x86_64; os = linux
        foo.1: Incompatible with restriction: = 2
  [1]

The SAT engine itself rejects the conflict; the post-solve version-conflict
check is no longer reached. The do_solve retry path runs SAT 3 times before
reporting the failure, and each run records the same cross-platform conflict.

  $ dune trace cat \
  > | jq -s 'include "dune"; [ .[] | satSolveEvents ] | length'
  3

No partial lock directory is written:
  $ test ! -e dune.lock
