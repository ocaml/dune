Running `dune pkg outdated` assumes package management is already enabled for a
project. If a user tries to run the subcommand without package management
enabled, they should get an informative user error.

  $ dune pkg outdated
  Error: Package management is not enabled in this project.
  Hint: Create a lock directory with 'dune pkg lock' or add (pkg enabled) to
  your dune-workspace file.
  [1]
