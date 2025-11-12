Detect common typos with package variables when describing platforms to solve for.

  $ . ../helpers.sh
  $ mkrepo
  $ add_mock_repo_if_needed

Create a workspace with some typos in package variable names
  $ cat > dune-workspace <<EOF
  > (lang dune 3.20)
  > (repository
  >  (name mock)
  >  (url "file://$(pwd)/mock-opam-repository"))
  > (lock_dir
  >  (repositories mock)
  >  (solve_for_platforms
  >   ((arch x86_64)
  >    (os linux)
  >    (os_distribution debian)
  >    (os_family debian))))
  > (pkg enabled)
  > EOF

  $ cat > dune-project <<EOF
  > (lang dune 3.18)
  > (package
  >  (name x)
  >  (allow_empty))
  > EOF

  $ DUNE_CONFIG__PORTABLE_LOCK_DIR=enabled dune pkg lock
  File "dune-workspace", line 10, characters 4-19:
  10 |    (os_distribution debian)
           ^^^^^^^^^^^^^^^
  Warning: The package variable "os_distribution" looks like a typo. Did you
  mean "os-distribution"?
  File "dune-workspace", line 11, characters 4-13:
  11 |    (os_family debian))))
           ^^^^^^^^^
  Warning: The package variable "os_family" looks like a typo. Did you mean
  "os-family"?
  Solution for dune.lock
  
  Dependencies common to all supported platforms:
  (none)
