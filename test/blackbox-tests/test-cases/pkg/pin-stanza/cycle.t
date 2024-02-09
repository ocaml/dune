Demonstrate a cycle from package sources:

CR-rgrinberg: the cycle checking is disabled for now because it's not clear if
it's even worth checking for. What matters is that there are no cycles at the
package level, the sources can contain a cycle, we just need to make sure we
detect it and not descend into an infinite loop.

  $ . ../helpers.sh

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

  $ runtest a
  Error: Dune does not support packages outside the workspace depending on
  packages in the workspace. The package "b" is not in the workspace but it
  depends on the package "a" which is in the workspace.
  [1]
  $ runtest b
  Error: Dune does not support packages outside the workspace depending on
  packages in the workspace. The package "a" is not in the workspace but it
  depends on the package "b" which is in the workspace.
  [1]
