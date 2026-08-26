Demonstrate that locking fails entirely when the requested platform set has no
joint solution: no partial lock directory is written.

The all-or-nothing behavior has landed: the lock now fails because the package
is unavailable on the linux platforms, and no lock directory is written. The
failure is still the product of per-platform solving and is reported per
platform; the single-solve change replaces it with one joint failure for the
requested platform set.

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
  
  The dependency solver failed to find a solution for the following platforms:
  - arch = x86_64; os = linux
  - arch = arm64; os = linux
  ...with this error:
  Couldn't solve the package dependency formula.
  Selected candidates: x.dev
  - foo -> (problem)
      No usable implementations:
        foo.0.0.1: Availability condition not satisfied
  [1]

No partial lock directory is written:

  $ test ! -e dune.lock

A package required on only one of several requested platforms fails only that
platform. The later single-solve change will list the full requested platform
set and qualify the package diagnostic:

  $ cat > dune-project <<EOF
  > (lang dune 3.18)
  > (package
  >  (name x)
  >  (depends
  >   (foo
  >    (and
  >     (= :arch x86_64)
  >     (= :os linux)))))
  > EOF

  $ dune pkg lock
  Error:
  Unable to solve dependencies while generating lock directory: dune.lock
  
  The dependency solver failed to find a solution for the following platforms:
  - arch = x86_64; os = linux
  ...with this error:
  Couldn't solve the package dependency formula.
  Selected candidates: x.dev
  - foo -> (problem)
      No usable implementations:
        foo.0.0.1: Availability condition not satisfied
  [1]

Identical failures on both Linux platforms are grouped under those two
platforms. Once a joint failure lists all four requested platforms, each
affected platform must instead be named on its package diagnostic:

  $ cat > dune-project <<EOF
  > (lang dune 3.18)
  > (package
  >  (name x)
  >  (depends
  >   (foo (= :os linux))))
  > EOF

  $ dune pkg lock
  Error:
  Unable to solve dependencies while generating lock directory: dune.lock
  
  The dependency solver failed to find a solution for the following platforms:
  - arch = x86_64; os = linux
  - arch = arm64; os = linux
  ...with this error:
  Couldn't solve the package dependency formula.
  Selected candidates: x.dev
  - foo -> (problem)
      No usable implementations:
        foo.0.0.1: Availability condition not satisfied
  [1]

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

  $ write_portable_lockdirs_project
  $ dune pkg lock
  Solution for dune.lock
  
  Dependencies common to all supported platforms:
  - foo.0.0.1
