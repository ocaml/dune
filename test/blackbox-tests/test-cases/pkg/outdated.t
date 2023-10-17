  $ . ./helpers.sh

  $ outdated () {
  >  dune pkg outdated --opam-repository-path=mock-opam-repository $@
  > }

`dune pkg outdated` lists the outdated packages in the current project.
  $ mkrepo
  $ mkpkg foo
  $ mkpkg bar <<EOF
  > depends: [ "foo" {>= "0.0.1"} ]
  > EOF
  $ cat > dune-workspace <<EOF
  > (lang dune 3.11)
  > (context
  >  (default))
  > (context
  >  (default
  >   (name workspace-context)
  >   (lock dune.workspace.lock)))
  > EOF
  $ solve_project --all-contexts <<EOF
  > (lang dune 3.11)
  > (package
  >  (name baz)
  >  (depends bar))
  > EOF
  Solution for dune.workspace.lock:
  - bar.0.0.1
  - foo.0.0.1
  Solution for dune.lock:
  - bar.0.0.1
  - foo.0.0.1
No package should be outdated after a fresh lock.
  $ outdated
  dune.lock is up to date.
Same for all contexts:
  $ outdated --all-contexts
  - dune.workspace.lock is up to date.
  - dune.lock is up to date.

Adding a new version of the bar package to the repository.
  $ mkpkg bar 0.0.2 <<EOF
  > depends: [ "foo" {>= "0.0.1"} ]
  > EOF

Dune should report the new version of bar as available.
  $ outdated
  1/2 packages in dune.lock are outdated.
  - bar 0.0.1 < 0.0.2
Same for all contexts:
  $ outdated --all-contexts
  - 1/2 packages in dune.workspace.lock are outdated.
    - bar 0.0.1 < 0.0.2
  - 1/2 packages in dune.lock are outdated.
    - bar 0.0.1 < 0.0.2

Now we add a new version of the foo package to the repository.
Dune should only report the bar package as it is an immediate dependency.
  $ mkpkg foo 0.0.2 
  $ outdated
  2/2 packages in dune.lock are outdated.
  Showing immediate dependencies, use --transitive to see them all.
  - bar 0.0.1 < 0.0.2
Same for all contexts:
  $ outdated --all-contexts
  - 2/2 packages in dune.workspace.lock are outdated.
    Showing immediate dependencies, use --transitive to see them all.
    - bar 0.0.1 < 0.0.2
  - 2/2 packages in dune.lock are outdated.
    Showing immediate dependencies, use --transitive to see them all.
    - bar 0.0.1 < 0.0.2

If --transitive is also passed then both should be reported.
  $ outdated --transitive
  2/2 packages in dune.lock are outdated.
  - bar 0.0.1 < 0.0.2
  - foo 0.0.1 < 0.0.2
Same for all contexts:
  $ outdated --all-contexts --transitive
  - 2/2 packages in dune.workspace.lock are outdated.
    - bar 0.0.1 < 0.0.2
    - foo 0.0.1 < 0.0.2
  - 2/2 packages in dune.lock are outdated.
    - bar 0.0.1 < 0.0.2
    - foo 0.0.1 < 0.0.2

If we remove packages from the repository then we should get a nice error.
  $ rm -rf mock-opam-repository/packages/bar
  $ outdated 
  1/2 packages in dune.lock are outdated.
  Showing immediate dependencies, use --transitive to see the rest.
  Error: Some packages could not be found.
  When checking dune.lock, the following packages:
  - bar
  were not found in the following opam repositories:
  - None
  [1]

When printing both successes and failures, any errors should appear afterwards.
  $ outdated --transitive
  1/2 packages in dune.lock are outdated.
  - foo 0.0.1 < 0.0.2
  Error: Some packages could not be found.
  When checking dune.lock, the following packages:
  - bar
  were not found in the following opam repositories:
  - None
  [1]

Similarly for all contexts.
  $ outdated --all-contexts --transitive
  - 1/2 packages in dune.workspace.lock are outdated.
    - foo 0.0.1 < 0.0.2
  - 1/2 packages in dune.lock are outdated.
    - foo 0.0.1 < 0.0.2
  Error: Some packages could not be found.
  When checking dune.workspace.lock, the following packages:
  - bar
  were not found in the following opam repositories:
  - None
  When checking dune.lock, the following packages:
  - bar
  were not found in the following opam repositories:
  - None
  [1]

If multiple packages are missing, the error should enumerate them. The errors should
appear irrespective of being a transitive dependency.
  $ rm -r mock-opam-repository/packages/foo
  $ outdated --transitive 
  dune.lock is up to date.
  Error: Some packages could not be found.
  When checking dune.lock, the following packages:
  - bar
  - foo
  were not found in the following opam repositories:
  - None
  [1]

With multiple contexts, the errors should also be printed for each context.
  $ outdated --all-contexts
  - dune.workspace.lock is up to date.
  - dune.lock is up to date.
  Error: Some packages could not be found.
  When checking dune.workspace.lock, the following packages:
  - bar
  - foo
  were not found in the following opam repositories:
  - None
  When checking dune.lock, the following packages:
  - bar
  - foo
  were not found in the following opam repositories:
  - None
  [1]
