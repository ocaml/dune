Solver diagnostics evaluate package availability in the failing platform's
environment.

  $ mkrepo
  $ add_mock_repo_if_needed

  $ mkpkg foo <<'EOF'
  > available: os != "linux"
  > EOF

  $ cat >dune-workspace <<EOF
  > (lang dune 3.20)
  > (repository
  >  (name mock)
  >  (url "file://$(pwd)/mock-opam-repository"))
  > (lock_dir
  >  (repositories mock)
  >  (solve_for_platforms
  >   ((arch x86_64)
  >    (os linux))))
  > (pkg enabled)
  > EOF
  $ write_portable_lockdirs_project

The package exists in the repository but is unavailable on Linux. The rejection
reason must be derived from Linux rather than a platform-less environment.

  $ dune pkg lock
  Error:
  Unable to solve dependencies while generating lock directory: dune.lock
  
  The dependency solver failed to find a solution for the requested platforms:
  - arch = x86_64; os = linux
  ...with this error:
  Couldn't solve the package dependency formula.
  Selected candidates: x.dev
  - foo -> (problem)
      No usable implementations:
        foo.0.0.1: Availability condition not satisfied
  [1]

Repository versions shadowed by a pin are not candidates and must not appear in
the rejection list. The pin is available only on Linux:

  $ cat >dune-workspace <<EOF
  > (lang dune 3.20)
  > (lock_dir
  >  (repositories mock)
  >  (solve_for_platforms
  >   ((arch x86_64) (os linux))
  >   ((arch arm64) (os linux))
  >   ((arch x86_64) (os macos))
  >   ((arch arm64) (os macos))))
  > (repository
  >  (name mock)
  >  (url "file://$(pwd)/mock-opam-repository"))
  > (pkg enabled)
  > EOF
  $ mkpkg bar <<EOF
  > available: os = "macos"
  > EOF
  $ mkdir _pinned-foo
  $ cat >_pinned-foo/foo.opam <<EOF
  > opam-version: "2.0"
  > available: os = "linux"
  > EOF
  $ cat >dune-project <<EOF
  > (lang dune 3.20)
  > (pin
  >  (url "file://$PWD/_pinned-foo")
  >  (package
  >   (name foo)
  >   (version 2)))
  > EOF
  $ cat >x.opam <<EOF
  > opam-version: "2.0"
  > depends: [ "foo" {= "2"} | "bar" ]
  > EOF

The local package can use the pin on Linux and the repository alternative on
macOS. The successful solve selects the pin only where it is available:

  $ dune pkg lock
  Solution for dune.lock
  
  Dependencies common to all supported platforms:
  (none)
  
  Additionally, some packages will only be built on specific platforms.
  
  arch = arm64; os = linux:
  - foo.2
  
  arch = arm64; os = macos:
  - bar.0.0.1
  
  arch = x86_64; os = linux:
  - foo.2
  
  arch = x86_64; os = macos:
  - bar.0.0.1

Removing the alternative makes the dependency fail on the two macOS platforms.
Only the pinned version appears in their rejection lists:

  $ cat >x.opam <<EOF
  > opam-version: "2.0"
  > depends: [ "foo" {= "2"} ]
  > EOF
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
  Selected candidates: foo.2 x.dev
  - foo -> (problem) on arch = arm64; os = macos
      No usable implementations:
        foo.2: Availability condition not satisfied
  - foo -> (problem) on arch = x86_64; os = macos
      No usable implementations:
        foo.2: Availability condition not satisfied
  [1]
