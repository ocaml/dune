A lock directory with one requested platform does not need cross-platform
version-consistency variables or clauses.

  $ mkrepo
  $ add_mock_repo_if_needed
  $ mkpkg foo

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

The input is fixed, so the exact SAT problem size detects redundant encoding.

  $ DUNE_TRACE=+sat dune pkg lock >/dev/null 2>&1
  $ dune trace cat | jq -s 'include "dune";
  > [ .[] | satSolveEvents
  >   | .args | { num_variables, num_clauses }
  > ]'
  [
    {
      "num_variables": 2,
      "num_clauses": 4
    }
  ]
