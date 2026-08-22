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
