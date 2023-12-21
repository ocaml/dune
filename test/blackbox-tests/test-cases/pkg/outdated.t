  $ . ./helpers.sh

  $ outdated () {
  >  dune pkg outdated $@
  > }

`dune pkg outdated` lists the outdated packages in the current project.
  $ mkrepo
  $ mkpkg foo
  $ mkpkg bar <<EOF
  > depends: [ "foo" {>= "0.0.1"} ]
  > EOF
  $ cat > dune-workspace <<EOF
  > (lang dune 3.11)
  > (lock_dir
  >  (repositories mock))
  > (lock_dir
  >  (path dune.workspace.lock)
  >  (repositories mock))
  > (context
  >  (default))
  > (context
  >  (default
  >   (name workspace-context)
  >   (lock_dir dune.workspace.lock)))
  > (repository
  >  (name mock)
  >  (source "file://$(pwd)/mock-opam-repository"))
  > EOF
  $ solve_project --all <<EOF
  > (lang dune 3.11)
  > (package
  >  (name baz)
  >  (depends bar))
  > EOF
  Solution for dune.lock:
  - bar.0.0.1
  - foo.0.0.1
  Solution for dune.workspace.lock:
  - bar.0.0.1
  - foo.0.0.1

No package should be outdated after a fresh lock.

Default behaviour is to check the default lock file.
  $ outdated
  dune.lock is up to date.

All lock file can be check by passing --all
  $ outdated --all
  - dune.lock is up to date.
  - dune.workspace.lock is up to date.

Specific lock files can be given as positional arguments.
  $ outdated dune.lock
  dune.lock is up to date.

Invalid lock files give an error
  $ outdated invalid_lock
  Error: The following directories are not lock directories in this workspace:
  - invalid_lock
  This workspace contains the following lock directories:
  - dune.lock
  - dune.workspace.lock
  [1]

Multiple lock files can be given.
  $ outdated dune.lock dune.workspace.lock
  - dune.lock is up to date.
  - dune.workspace.lock is up to date.

Adding a new version of the bar package to the repository.

  $ mkpkg bar 0.0.2 <<EOF
  > depends: [ "foo" {>= "0.0.1"} ]
  > EOF

Dune should report the new version of bar as available.

  $ outdated --all
  - 1/2 packages in dune.lock are outdated.
    - bar 0.0.1 < 0.0.2
  - 1/2 packages in dune.workspace.lock are outdated.
    - bar 0.0.1 < 0.0.2

Now we add a new version of the foo package to the repository.
Dune should only report the bar package as it is an immediate dependency.

  $ mkpkg foo 0.0.2 
  $ outdated --all
  - 2/2 packages in dune.lock are outdated.
    Showing immediate dependencies, use --transitive to see them all.
    - bar 0.0.1 < 0.0.2
  - 2/2 packages in dune.workspace.lock are outdated.
    Showing immediate dependencies, use --transitive to see them all.
    - bar 0.0.1 < 0.0.2

If --transitive is also passed then both should be reported.

  $ outdated --transitive dune.lock
  2/2 packages in dune.lock are outdated.
  - bar 0.0.1 < 0.0.2
  - foo 0.0.1 < 0.0.2

If we remove packages from the repository then we should get a nice error.

  $ rm -rf mock-opam-repository/packages/bar
  $ outdated dune.lock 
  1/2 packages in dune.lock are outdated.
  Showing immediate dependencies, use --transitive to see the rest.
  Error: Some packages could not be found.
  When checking dune.lock, the following packages:
  - bar
  were not found in the following opam repositories:
  - None
  [1]

When printing both successes and failures, any errors should appear afterwards.

  $ outdated dune.lock --transitive
  1/2 packages in dune.lock are outdated.
  - foo 0.0.1 < 0.0.2
  Error: Some packages could not be found.
  When checking dune.lock, the following packages:
  - bar
  were not found in the following opam repositories:
  - None
  [1]

Similarly for multiple lock files.

  $ outdated --transitive --all
  - 1/2 packages in dune.lock are outdated.
    - foo 0.0.1 < 0.0.2
  - 1/2 packages in dune.workspace.lock are outdated.
    - foo 0.0.1 < 0.0.2
  Error: Some packages could not be found.
  When checking dune.lock, the following packages:
  - bar
  were not found in the following opam repositories:
  - None
  When checking dune.workspace.lock, the following packages:
  - bar
  were not found in the following opam repositories:
  - None
  [1]

If multiple packages are missing, the error should enumerate them. The errors should
appear irrespective of being a transitive dependency.

  $ rm -r mock-opam-repository/packages/foo
  $ outdated --transitive dune.lock 
  dune.lock is up to date.
  Error: Some packages could not be found.
  When checking dune.lock, the following packages:
  - bar
  - foo
  were not found in the following opam repositories:
  - None
  [1]

With multiple lock files, the errors should also be printed for each of them.

  $ outdated --all 
  - dune.lock is up to date.
  - dune.workspace.lock is up to date.
  Error: Some packages could not be found.
  When checking dune.lock, the following packages:
  - bar
  - foo
  were not found in the following opam repositories:
  - None
  When checking dune.workspace.lock, the following packages:
  - bar
  - foo
  were not found in the following opam repositories:
  - None
  [1]
