Package sources can be set to git:

  $ mkrepo
  $ add_mock_repo_if_needed

We create a repo with a fixed name for the default branch.

  $ mkdir _repo
  $ cd _repo
  $ git init --initial-branch=duplicated --quiet
  $ cat >dune-project <<EOF
  > (lang dune 3.13)
  > (package (name foo))
  > EOF
  $ git add -A
  $ git commit -qm "initial commit"
  $ cd ..

  $ cat >dune-project <<EOF
  > (lang dune 3.13)
  > (pin
  >  (url "git+file://$PWD/_repo")
  >  (package (name foo)))
  > (package
  >  (name main)
  >  (depends foo))
  > EOF

  $ dune_pkg_lock_normalized
  Solution for dune.lock:
  - foo.dev

We create a tag that clashes with the name of the branch (hence we needed to
fix the name of the branch eariler):

  $ git -C _repo tag duplicated

This should work without issue, as we never reference the ambiguous reference:

  $ dune_pkg_lock_normalized
  Solution for dune.lock:
  - foo.dev

If we use the duplicate reference in the config

  $ cat >dune-project <<EOF
  > (lang dune 3.13)
  > (pin
  >  (url "git+file://$PWD/_repo#duplicated")
  >  (package (name foo)))
  > (package
  >  (name main)
  >  (depends foo))
  > EOF

This will work as both references point at the same revision, thus aren't
ambiguous:

  $ dune_pkg_lock_normalized
  Solution for dune.lock:
  - foo.dev

If we then change the reference of the branch to point to a different revision
than the tag is pointing to (still the initial commit):

  $ git -C _repo commit --quiet --allow-empty --message "New ref"

In this case Dune can't determine which reference to use and will error out:

  $ dune_pkg_lock_normalized 2>&1 | dune_cmd subst "$PWD" '$PWD'
  Error: Reference "duplicated" in remote
  "file://$PWD/_repo"
  is ambiguous
  Hint: If you want to specify a tag use refs/tags/duplicated
  Hint: If you want to specify a branch use refs/branches/duplicated
  [1]

Git also has unambibuous namespaces tags and branches, for tags it is `refs/tags/`.

  $ cat >dune-project <<EOF
  > (lang dune 3.13)
  > (pin
  >  (url "git+file://$PWD/_repo#refs/tags/duplicated")
  >  (package (name foo)))
  > (package
  >  (name main)
  >  (depends foo))
  > EOF

Locking should work, as there are no ambiguous references.

  $ dune_pkg_lock_normalized
  Solution for dune.lock:
  - foo.dev

For branches the namespace is `refs/heads/`:

  $ cat >dune-project <<EOF
  > (lang dune 3.13)
  > (pin
  >  (url "git+file://$PWD/_repo#refs/heads/duplicated")
  >  (package (name foo)))
  > (package
  >  (name main)
  >  (depends foo))
  > EOF

Likewise locking a branch this way should work as well:

  $ dune_pkg_lock_normalized
  Solution for dune.lock:
  - foo.dev
