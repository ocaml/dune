It should be possible to include custom repos from the workspace:

  $ mkrepo
  $ add_mock_repo_if_needed

  $ mkdir _foo
  $ cat >_foo/dune-project <<EOF
  > (lang dune 3.13)
  > (package (name foo))
  > EOF

  $ cat >dune-workspace<<EOF
  > (lang dune 3.10)
  > (pin
  >  (name foo)
  >  (url "file://$PWD/_foo")
  >  (package (name foo)))
  > (lock_dir
  >  (pins foo)
  >  (repositories mock))
  > (repository
  >  (name mock)
  >  (url "file://$(pwd)/mock-opam-repository"))
  > EOF

  $ mkrepo

Note that sources in the projects are overriden by the workspace

  $ cat >dune-project <<EOF
  > (lang dune 3.13)
  > (pin ;; does not exist
  >  (url "file://$PWD/_does_not_exist")
  >  (package (name foo)))
  > (package
  >  (name main)
  >  (depends foo))
  > EOF

  $ dune_pkg_lock_normalized
  Solution for dune.lock:
  - foo.dev

Workspace pins need names so lock directories can refer to them explicitly.

  $ mkdir missing-name
  $ cd missing-name

  $ cat >dune-project <<EOF
  > (lang dune 3.13)
  > (package
  >  (name main)
  >  (allow_empty))
  > EOF

  $ cat >dune-workspace <<EOF
  > (lang dune 3.13)
  > (pin
  >  (url "file://$PWD/_foo")
  >  (package (name foo)))
  > EOF

  $ dune pkg lock 2>&1 \
  > | dune_cmd subst 'characters 0-[0-9]+:' 'characters 0-N:' \
  > | dune_cmd subst "$PWD" '$PWD'
  File "dune-workspace", lines 2-4, characters 0-N:
  2 | (pin
  3 |  (url "file://$PWD/_foo")
  4 |  (package (name foo)))
  Error: Pin stanzas in dune-workspace must have a name.
  Workspace pins are named so lock_dir stanzas can choose which pins to use.
  Hint: Add a (name <pin>) field to this pin and list the name in the relevant
  lock_dir's (pins ...) field.
  [1]
