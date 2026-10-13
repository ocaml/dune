Test that "dune describe pkg tree" recursively displays the dependency tree of
each local package.

  $ mkrepo

  $ mkpkg a 0.0.1 <<EOF
  > EOF
  $ mkpkg b <<EOF
  > EOF
  $ mkpkg c <<EOF
  > EOF
  $ mkpkg bar <<EOF
  > depends: [ "b" "c"]
  > EOF
  $ mkpkg baz <<EOF
  > depends: [ "bar" ]
  > EOF
  $ mkpkg qux <<EOF
  > depends: [ "a" "b" "c" ]
  > EOF
  $ solve_project <<EOF
  > (lang dune 3.11)
  > (package
  >  (name local_1)
  >  (depends
  >   local_2))
  > (package
  >  (name local_2)
  >  (depends
  >   baz
  >   (qux :with-test)))
  > EOF
  Solution for dune.lock:
  - a.0.0.1
  - b.0.0.1
  - bar.0.0.1
  - baz.0.0.1
  - c.0.0.1
  - qux.0.0.1

The tree is printed recursively. Each package's subtree is expanded only the
first time it is encountered; later occurrences print just the package, and the
number of times a package occurs in the full tree is shown as "(*N)". For
example "b" and "c" occur 4 times each, and "local_2" is both a local package
and a dependency of "local_1" so it occurs twice.

  $ dune describe pkg tree
  Dependency tree of local packages locked in _build/_private/default/.lock/dune.lock
  - local_1.dev
    - local_2.dev (*2)
      - baz.0.0.1 (*2)
        - bar.0.0.1 (*2)
          - b.0.0.1 (*4)
          - c.0.0.1 (*4)
      - qux.0.0.1 (*2)
        - a.0.0.1 (*2)
        - b.0.0.1 (*4)
        - c.0.0.1 (*4)
  - local_2.dev (*2)

Two local packages that are both roots, where "proj_a" depends on "proj_b". The
subtree of "proj_b" is expanded under "proj_a"; its second, top-level occurrence
is collapsed to a leaf. This confirms top-level roots are collapsed like any
other repeated package.

  $ solve_project <<EOF
  > (lang dune 3.11)
  > (package (name proj_a) (depends proj_b))
  > (package (name proj_b) (depends c))
  > EOF
  Solution for dune.lock:
  - c.0.0.1
  $ dune describe pkg tree
  Dependency tree of local packages locked in _build/_private/default/.lock/dune.lock
  - proj_a.dev
    - proj_b.dev (*2)
      - c.0.0.1 (*2)
  - proj_b.dev (*2)

A local package ("alpha") depending on a locked package ("beta") that in turn
depends back on a local package ("gamma") is not a supported workspace: dune
rejects a package outside the workspace depending on one inside it, so this
topology never reaches the dependency tree.

  $ mkpkg beta <<EOF
  > depends: [ "gamma" ]
  > EOF
  $ solve_project <<EOF
  > (lang dune 3.11)
  > (package (name alpha) (depends beta))
  > (package (name gamma))
  > EOF
  Error: Dune does not support packages outside the workspace depending on
  packages in the workspace. The package "beta" is not in the workspace but it
  depends on the package "gamma" which is in the workspace.
  [1]

A dependency cycle between two local packages. The graph tolerates cycles: each
package is expanded once and the edge that closes the cycle is marked "(cycle)"
instead of looping. Occurrence counts are not shown for packages involved in a
cycle, where they are not well-defined.

  $ solve_project <<EOF
  > (lang dune 3.11)
  > (package (name left) (depends right))
  > (package (name right) (depends left))
  > EOF
  Solution for dune.lock:
  (no dependencies to lock)
  $ dune describe pkg tree
  Dependency tree of local packages locked in _build/_private/default/.lock/dune.lock
  - left.dev
    - right.dev
      - left.dev (cycle)
  - right.dev

A local package "app" with ten dependencies that all share a common dependency
"x". "x" is expanded once and its occurrence count reflects that it is reached
through all ten dependencies.

  $ mkpkg x <<EOF
  > EOF
  $ for i in 0 1 2 3 4 5 6 7 8 9; do
  >   echo 'depends: [ "x" ]' | mkpkg dep$i
  > done
  $ solve_project <<EOF
  > (lang dune 3.11)
  > (package
  >  (name app)
  >  (depends dep0 dep1 dep2 dep3 dep4 dep5 dep6 dep7 dep8 dep9))
  > EOF
  Solution for dune.lock:
  - dep0.0.0.1
  - dep1.0.0.1
  - dep2.0.0.1
  - dep3.0.0.1
  - dep4.0.0.1
  - dep5.0.0.1
  - dep6.0.0.1
  - dep7.0.0.1
  - dep8.0.0.1
  - dep9.0.0.1
  - x.0.0.1
  $ dune describe pkg tree
  Dependency tree of local packages locked in _build/_private/default/.lock/dune.lock
  - app.dev
    - dep0.0.0.1
      - x.0.0.1 (*10)
    - dep1.0.0.1
      - x.0.0.1 (*10)
    - dep2.0.0.1
      - x.0.0.1 (*10)
    - dep3.0.0.1
      - x.0.0.1 (*10)
    - dep4.0.0.1
      - x.0.0.1 (*10)
    - dep5.0.0.1
      - x.0.0.1 (*10)
    - dep6.0.0.1
      - x.0.0.1 (*10)
    - dep7.0.0.1
      - x.0.0.1 (*10)
    - dep8.0.0.1
      - x.0.0.1 (*10)
    - dep9.0.0.1
      - x.0.0.1 (*10)

A workspace with a single local package and no dependencies at all: the tree is
just the header and one leaf line.

  $ solve_project <<EOF
  > (lang dune 3.11)
  > (package (name solo))
  > EOF
  Solution for dune.lock:
  (no dependencies to lock)
  $ dune describe pkg tree
  Dependency tree of local packages locked in _build/_private/default/.lock/dune.lock
  - solo.dev

A cycle deeper in the tree: "a_top" depends on "b_mid", "b_mid" on "c_bot", and
"c_bot" both closes the cycle back to "b_mid" and depends on the locked package
"x" below the cycle. The cycle edge is marked at depth 3 and "x" is still
printed; occurrence counts are suppressed for every package affected by the
cycle ("a_top" is unaffected).

  $ solve_project <<EOF
  > (lang dune 3.11)
  > (package (name a_top) (depends b_mid))
  > (package (name b_mid) (depends c_bot))
  > (package (name c_bot) (depends b_mid x))
  > EOF
  Solution for dune.lock:
  - x.0.0.1
  $ dune describe pkg tree
  Dependency tree of local packages locked in _build/_private/default/.lock/dune.lock
  - a_top.dev
    - b_mid.dev
      - c_bot.dev
        - b_mid.dev (cycle)
        - x.0.0.1
  - b_mid.dev
  - c_bot.dev

A package that depends on itself: the tightest possible cycle, a self-loop
edge.

  $ solve_project <<EOF
  > (lang dune 3.11)
  > (package (name selfy) (depends selfy))
  > EOF
  Solution for dune.lock:
  (no dependencies to lock)
  $ dune describe pkg tree
  Dependency tree of local packages locked in _build/_private/default/.lock/dune.lock
  - selfy.dev
    - selfy.dev (cycle)

A cycle of three packages: "ring_a" depends on "ring_b", which depends on
"ring_c", which closes the cycle back to "ring_a".

  $ solve_project <<EOF
  > (lang dune 3.11)
  > (package (name ring_a) (depends ring_b))
  > (package (name ring_b) (depends ring_c))
  > (package (name ring_c) (depends ring_a))
  > EOF
  Solution for dune.lock:
  (no dependencies to lock)
  $ dune describe pkg tree
  Dependency tree of local packages locked in _build/_private/default/.lock/dune.lock
  - ring_a.dev
    - ring_b.dev
      - ring_c.dev
        - ring_a.dev (cycle)
  - ring_b.dev
  - ring_c.dev

A workspace with no local packages at all: the tree below the header is empty.

  $ solve_project <<EOF
  > (lang dune 3.11)
  > EOF
  Solution for dune.lock:
  (no dependencies to lock)
  $ dune describe pkg tree
  Dependency tree of local packages locked in _build/_private/default/.lock/dune.lock
  

With package management enabled and no committed lockdir, "dune describe pkg
tree" autolocks on demand and displays the generated lockdir's dependency tree.

  $ mkdir autolock
  $ cd autolock
  $ mkrepo
  $ mkpkg shared <<EOF
  > EOF
  $ mkpkg dep <<EOF
  > depends: [ "shared" ]
  > EOF
  $ add_mock_repo_if_needed
  $ cat > dune-project <<EOF
  > (lang dune 3.20)
  > (package
  >  (name app)
  >  (depends dep))
  > EOF
  $ enable_pkg
  $ dune describe pkg tree
  Dependency tree of local packages locked in _build/_private/default/.lock/dune.lock
  - app.dev
    - dep.0.0.1
      - shared.0.0.1
