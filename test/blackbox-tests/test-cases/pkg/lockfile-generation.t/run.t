Simple example of generating a lock file with Dune

Helper shell function that generates an opam file for a package:

  $ emptypkg() {
  >   mkpkg $1 <<EOF
  > EOF
  > }
  $ emptyverpkg() {
  >   mkpkg $1 $2 <<EOF
  > EOF
  > }

Generate a `dune-project` file.
  $ cat >dune-project <<EOF
  > (lang dune 3.8)
  > (package
  >  (name lockfile_generation_test)
  >  (depends
  >    foo
  >    (bar (>= "0.3"))))
  > EOF
  > mkpkg foo <<EOF
  > depends: [
  >     "baz" {>= "0.1"}
  >     "bar" {>= "0.2"}
  > ]
  > EOF
  $ add_mock_repo_if_needed

Run the solver and generate a lock directory.

  $ dune_pkg_lock_normalized
  Solution for dune.lock:
  - bar.0.5.0
  - baz.0.1.0
  - foo.0.0.1

Helper to the name and contents of each file in the lock directory separated by
"---", sorting by filename for consistency.
  $ print_all() { find ${default_lock_dir} -type f | sort | xargs -I{} sh -c "printf '{}:\n\n'; cat {}; printf '\n\n---\n\n'"; }

Print the contents of each file in the lockdir:
  $ print_all
  dune.lock/bar.0.5.0.pkg:
  
  (version 0.5.0)
  
  
  ---
  
  dune.lock/baz.0.1.0.pkg:
  
  (version 0.1.0)
  
  
  ---
  
  dune.lock/foo.0.0.1.pkg:
  
  (version 0.0.1)
  
  (depends
   (all_platforms
    (baz bar)))
  
  
  ---
  
  dune.lock/lock.dune:
  
  (lang package 0.1)
  
  (dependency_hash 8940266fc1693f5f2a008d1d58c1e1b9)
  
  (repositories
   (complete false)
   (used))
  
  (solved_for_platforms
   ((arch x86_64)
    (os linux))
   ((arch arm64)
    (os linux))
   ((arch x86_64)
    (os macos))
   ((arch arm64)
    (os macos)))
  
  
  ---
  

Run the solver again preferring oldest versions of dependencies:
  $ dune_pkg_lock_normalized --version-preference=oldest
  Solution for dune.lock:
  - bar.0.4.0
  - baz.0.1.0
  - foo.0.0.1

  $ print_all
  dune.lock/bar.0.4.0.pkg:
  
  (version 0.4.0)
  
  
  ---
  
  dune.lock/baz.0.1.0.pkg:
  
  (version 0.1.0)
  
  
  ---
  
  dune.lock/foo.0.0.1.pkg:
  
  (version 0.0.1)
  
  (depends
   (all_platforms
    (baz bar)))
  
  
  ---
  
  dune.lock/lock.dune:
  
  (lang package 0.1)
  
  (dependency_hash 8940266fc1693f5f2a008d1d58c1e1b9)
  
  (repositories
   (complete false)
   (used))
  
  (solved_for_platforms
   ((arch x86_64)
    (os linux))
   ((arch arm64)
    (os linux))
   ((arch x86_64)
    (os macos))
   ((arch arm64)
    (os macos)))
  
  
  ---
  

Regenerate the `dune-project` file introducing an unsatisfiable constraint.
  $ cat >dune-project <<EOF
  > (lang dune 3.8)
  > (package
  >  (name lockfile_generation_test)
  >  (depends
  >    foo
  >    (bar (>= "0.6"))))
  > EOF

Run the solver again. This time it will fail.
  $ dune_pkg_lock_normalized
  Error:
  Unable to solve dependencies while generating lock directory: dune.lock
  
  Couldn't solve the package dependency formula.
  Selected candidates: baz.0.1.0 foo.0.0.1 lockfile_generation_test.dev
  - bar -> (problem)
      No usable implementations:
        bar.0.5.0:
          Package does not satisfy constraints of local package
          lockfile_generation_test
        bar.0.4.0:
          Package does not satisfy constraints of local package
          lockfile_generation_test
        bar.0.0.1:
          Package does not satisfy constraints of local package
          lockfile_generation_test
  [1]

We'll also test how the lockfile generation works with alternate solutions.
`bar-or-baz` is a package that depends on either `bar` or `baz` and the solver
should pick one of them.

  $ cat >dune-project <<EOF
  > (lang dune 3.8)
  > (package
  >  (name lockfile_generation_test)
  >  (depends bar-or-baz))
  > EOF
  $ mkpkg bar-or-baz <<EOF
  > depends: [ "bar" | "baz" ]
  > EOF

After running this we expact a solution that has either `bar` or `baz` but not
both.

  $ dune_pkg_lock_normalized
  Solution for dune.lock:
  - bar.0.5.0
  - bar-or-baz.0.0.1
Top level or is simple, but does nested or work? nested-r defines nested or
patterns that can't be simplified

  $ cat >dune-project <<EOF
  > (lang dune 3.8)
  > (package
  >  (name lockfile_generation_test)
  >  (depends nested-or))
  > EOF
  $ emptypkg qux
  > emptypkg quz
  $ emptypkg quux
  $ emptypkg corge
  $ mkpkg nested-or <<EOF
  > depends: [ "quux" (("baz" | "quz") & ("bar" | "qux")) ]
  > EOF

After runninng we expect the solution to have quux and either baz or quz as
well as bar or qux.

  $ dune_pkg_lock_normalized
  Solution for dune.lock:
  - bar.0.5.0
  - baz.0.1.0
  - nested-or.0.0.1
  - quux.0.0.1
In the dependency formulas, & should bind stronger than | so if we depend on
bar and quux or baz, it should pick the first two or the last one, but nothing
in between.

  $ cat >dune-project <<EOF
  > (lang dune 3.8)
  > (package
  >  (name lockfile_generation_test)
  >  (depends priorities))
  > EOF
  $ mkpkg priorities <<EOF
  > depends: [ ("bar" & "quux") | "baz" ]
  > EOF

  $ dune_pkg_lock_normalized
  Solution for dune.lock:
  - bar.0.5.0
  - priorities.0.0.1
  - quux.0.0.1
 
We also want to make sure nested negation in versions work fine. For this we
have the same package with version 1-4 and we want to negate the choice of
versions 1 or 3, as well as making sure it doesn't pick the newest version.
 
  $ emptyverpkg pkg 1
  $ emptyverpkg pkg 2
  $ emptyverpkg pkg 3
  $ emptyverpkg pkg 4
  $ cat >dune-project <<EOF
  > (lang dune 3.8)
  > (package
  >  (name lockfile_generation_test)
  >  (depends negation))
  > EOF
  $ mkpkg negation <<EOF
  > depends: [ "pkg" {!((= "1") | (= "3")) & (< "4")} ]
  > EOF

With versions 1 and 3 negated and version 4 removed via version constraint,
we'd expect version 2 to be chosen:

  $ dune_pkg_lock_normalized
  Solution for dune.lock:
  - negation.0.0.1
  - pkg.2
