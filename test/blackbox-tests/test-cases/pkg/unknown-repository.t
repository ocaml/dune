The lock_dir references a repository that is not declared in the
workspace:

  $ cat > dune-project <<EOF
  > (lang dune 3.20)
  > (package (name foo))
  > EOF

  $ cat > dune-workspace <<EOF
  > (lang dune 3.20)
  > (lock_dir
  >  (repositories mock))
  > EOF

  $ dune pkg lock
  File "dune-workspace", line 3, characters 15-19:
  3 |  (repositories mock))
                     ^^^^
  Error: Repository 'mock' is not a known repository
  [1]

Declaring the repository in the workspace fixes the error:

  $ mkrepo
  $ cat > dune-workspace <<EOF
  > (lang dune 3.20)
  > (lock_dir
  >  (repositories mock))
  > (repository
  >  (name mock)
  >  (url "$(default_repo_path)"))
  > EOF

  $ dune pkg lock
  Solution for dune.lock
  
  Dependencies common to all supported platforms:
  (none)

