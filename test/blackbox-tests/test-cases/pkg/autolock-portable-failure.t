Test that an autolock failure for a portable lock dir suggests narrowing the
requested platform set.

  $ mkrepo
  $ add_mock_repo_if_needed
  $ enable_pkg

Make a package that is only available on macos:
  $ mkpkg foo <<EOF
  > available: os = "macos"
  > build: [
  >   ["mkdir" "-p" "%{lib}%/%{name}%"]
  >   ["touch" "%{lib}%/%{name}%/META"] # needed for dune to recognize this as a library
  > ]
  > EOF

Make a portable project that depends on it:
  $ make_portable_lockdirs_project

The default platform set includes linux, where "foo" cannot be installed, so
building fails and suggests narrowing the platform set. The error is
reported for the lock rule's internal target, so the path is normalized:
  $ dune build 2>&1 | dune_cmd subst 'default/.lock/_unknown_' 'dune.lock'
  File "dune.lock", line 1, characters 0-0:
  Error: Couldn't solve the package dependency formula.
  Selected candidates: foo.0.0.1 x.dev
  - foo -> (problem) on arch = arm64; os = linux
      No usable implementations:
        foo.0.0.1: Availability condition not satisfied
  - foo -> (problem) on arch = x86_64; os = linux
      No usable implementations:
        foo.0.0.1: Availability condition not satisfied
  Hint: If you don't need support for every requested platform, change
  Hint: (solve_for_platforms ...) in dune-workspace to only include the
  Hint: platforms you need, then rerun 'dune pkg lock'
  [1]
