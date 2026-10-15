There is no precedence between solver_env and explicit solve_for_platforms:
their variables must be disjoint.

  $ mkrepo
  $ add_mock_repo_if_needed

  $ mkpkg linux-only <<'EOF'
  > available: os = "linux"
  > EOF
  $ mkpkg macos-only <<'EOF'
  > available: os = "macos"
  > EOF

  $ cat >dune-project <<'EOF'
  > (lang dune 3.18)
  > EOF
  $ cat >x.opam <<'EOF'
  > opam-version: "2.0"
  > depends: [
  >   "linux-only" {os = "linux"}
  >   "macos-only" {os = "macos"}
  > ]
  > EOF
  $ cat >dune-workspace <<EOF
  > (lang dune 3.20)
  > (repository
  >  (name mock)
  >  (url "file://$(pwd)/mock-opam-repository"))
  > (lock_dir
  >  (repositories mock)
  >  (solver_env (with-doc true))
  >  (solve_for_platforms
  >   ((arch x86_64) (os linux) (with-doc true))
  >   ((arch x86_64) (os macos))))
  > (pkg enabled)
  > EOF

  $ DUNE_CONFIG__PORTABLE_LOCK_DIR=enabled dune pkg lock
  File "dune-workspace", lines 5-10, characters 0-158:
   5 | (lock_dir
   6 |  (repositories mock)
   7 |  (solver_env (with-doc true))
   8 |  (solve_for_platforms
   9 |   ((arch x86_64) (os linux) (with-doc true))
  10 |   ((arch x86_64) (os macos))))
  Error: Variable "with-doc" appears in both 'solver_env' and
  'solve_for_platforms', which is not allowed.
  [1]

Variables cannot be unset globally while being set by an explicit platform.

  $ cat >dune-workspace <<EOF
  > (lang dune 3.20)
  > (repository
  >  (name mock)
  >  (url "file://$(pwd)/mock-opam-repository"))
  > (lock_dir
  >  (repositories mock)
  >  (unset_solver_vars os)
  >  (solve_for_platforms
  >   ((arch x86_64) (os linux))
  >   ((arch x86_64) (os macos))))
  > (pkg enabled)
  > EOF

  $ DUNE_CONFIG__PORTABLE_LOCK_DIR=enabled dune pkg lock
  File "dune-workspace", lines 5-10, characters 0-136:
   5 | (lock_dir
   6 |  (repositories mock)
   7 |  (unset_solver_vars os)
   8 |  (solve_for_platforms
   9 |   ((arch x86_64) (os linux))
  10 |   ((arch x86_64) (os macos))))
  Error: Variable "os" appears in both 'unset_solver_vars' and
  'solve_for_platforms', which is not allowed.
  [1]

Disjoint variables remain valid.

  $ cat >dune-workspace <<EOF
  > (lang dune 3.20)
  > (repository
  >  (name mock)
  >  (url "file://$(pwd)/mock-opam-repository"))
  > (lock_dir
  >  (repositories mock)
  >  (solver_env (with-doc true))
  >  (solve_for_platforms
  >   ((arch x86_64) (os linux))
  >   ((arch x86_64) (os macos))))
  > (pkg enabled)
  > EOF

  $ DUNE_CONFIG__PORTABLE_LOCK_DIR=enabled dune pkg lock >/dev/null 2>&1
