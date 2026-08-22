Dependency cycles reachable from the local packages are user-writable
input: hand-written lock directories may contain them, and workspace
packages may depend on each other. Neither is diagnosed today.

First, a cycle between lock-dir packages. Generate a valid lockdir:

  $ mkrepo
  $ mkpkg a <<EOF
  > depends: [ "b" ]
  > EOF
  $ mkpkg b
  $ solve_project <<EOF
  > (lang dune 3.20)
  > (package
  >  (name x)
  >  (allow_empty)
  >  (depends a))
  > EOF
  Solution for dune.lock:
  - a.0.0.1
  - b.0.0.1

Complete the cycle by making locked package b depend on a. Validation
silently tolerates the cycle, deferring the failure to build time:

  $ cat > dune.lock/b.0.0.1.pkg <<EOF
  > (version 0.0.1)
  > (depends (all_platforms (a)))
  > EOF

  $ dune pkg validate-lockdir

Second, a cycle between workspace packages. Validation accepts it, and
querying the transitive dependencies tolerates it as well:

  $ cd ..
  $ mkdir workspace-cycle
  $ cd workspace-cycle
  $ mkrepo
  $ solve_project <<EOF
  > (lang dune 3.20)
  > (package
  >  (name a)
  >  (allow_empty)
  >  (depends b))
  > (package
  >  (name b)
  >  (allow_empty)
  >  (depends a))
  > EOF
  Solution for dune.lock:
  (no dependencies to lock)

  $ dune pkg validate-lockdir
  $ dune describe pkg list-locked-dependencies --transitive
  Dependencies of local packages locked in dune.lock
  - Transitive dependencies of local package a.dev
    - b.dev
    
  - Transitive dependencies of local package b.dev
    - a.dev
    
