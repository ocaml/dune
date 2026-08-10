Demonstrate that locking fails entirely when the requested platform set has no
joint solution: no partial lock directory is written.

  $ mkrepo
  $ add_mock_repo_if_needed

Make a package that is only available on macos.
  $ mkpkg foo <<EOF
  > available: os = "macos"
  > build: [
  >   ["mkdir" "-p" "%{lib}%/%{name}%"]
  >   ["touch" "%{lib}%/%{name}%/META"] # needed for dune to recognize this as a library
  > ]
  > EOF

  $ make_portable_lockdirs_project

The default platform set includes linux, where "foo" cannot be installed. The
failure is reported once for the requested platform set:

  $ dune pkg lock
  Error:
  Unable to solve dependencies while generating lock directory: dune.lock
  
  The dependency solver failed to find a solution for the requested platforms:
  - arch = x86_64; os = linux
  - arch = arm64; os = linux
  - arch = x86_64; os = macos
  - arch = arm64; os = macos
  ...with this error:
  Couldn't solve the package dependency formula.
  Selected candidates: foo.0.0.1 x.dev
  
  Hint: If you don't need support for every requested platform, change
  Hint: (solve_for_platforms ...) in dune-workspace to only include the
  Hint: platforms you need, then rerun 'dune pkg lock'
  [1]

No partial lock directory is written:

  $ test ! -e dune.lock

When the platform set only contains platforms where the package is available,
locking succeeds:

  $ cat > dune-workspace <<EOF
  > (lang dune 3.11)
  > (repository
  >  (name mock)
  >  (url "file://$(pwd)/mock-opam-repository"))
  > (lock_dir
  >  (repositories mock)
  >  (solve_for_platforms
  >   ((arch arm64)
  >    (os macos))))
  > EOF

  $ dune pkg lock
  Solution for dune.lock
  
  Dependencies common to all supported platforms:
  - foo.0.0.1
