Demonstrate a cycle from package sources:

CR-rgrinberg: the cycle checking is disabled for now because it's not clear if
it's even worth checking for. What matters is that there are no cycles at the
package level, the sources can contain a cycle, we just need to make sure we
detect it and not descend into an infinite loop.

  $ mkdir a b

  $ cat >a/dune-project <<EOF
  > (lang dune 3.13)
  > (pin
  >  (url "$PWD/b")
  >  (package (name b)))
  > (package
  >  (name a)
  >  (depends b))
  > EOF

  $ cat >b/dune-project <<EOF
  > (lang dune 3.13)
  > (pin
  >  (url "$PWD/a")
  >  (package (name a)))
  > (package
  >  (name b)
  >  (depends a))
  > EOF

  $ runtest() {
  > local res;
  > res=$(cd $1 && mkrepo && add_mock_repo_if_needed && dune pkg lock 2>&1)
  > local code=$?
  > printf "$res" $res
  > return $code
  > }

A locked package may depend on a workspace package, so each solve succeeds.
Cycles are not looked for when locking.

  $ runtest a
  Solution for dune.lock
  
  Dependencies common to all supported platforms:
  - b.dev
  $ runtest b
  Solution for dune.lock
  
  Dependencies common to all supported platforms:
  - a.dev

The cycle is found when building:

  $ (cd a && dune build @pkg-install 2>&1)
  Error: Dependency cycle between packages:
     a
  -> b
  -> a
  -> required by _build/default/.pkg/b/_install
  -> required by alias pkg-install
  [1]
