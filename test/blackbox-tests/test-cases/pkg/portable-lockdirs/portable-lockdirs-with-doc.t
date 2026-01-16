Test that the with-doc variable is stored in the lockdir when it's set in
dune-workspace.

  $ mkrepo
  $ add_mock_repo_if_needed

  $ cat > dune-workspace <<EOF
  > (lang dune 3.20)
  > (pkg enabled)
  > (lock_dir
  >  (repositories mock)
  >  (solver_env
  >   (with-doc true)))
  > (repository
  >  (name mock)
  >  (url "$PWD/mock-opam-repository"))
  > EOF

  $ cat > dune-project <<EOF
  > (lang dune 3.18)
  > (package
  >  (name x)
  >  (depends (foo :with-doc)))
  > EOF

  $ mkpkg foo

  $ DUNE_CONFIG__PORTABLE_LOCK_DIR=enabled dune pkg lock
  Solution for dune.lock
  
  Dependencies common to all supported platforms:
  - foo.0.0.1

The list-locked-dependencies command does some validation that there are no
extraneous packages in the lockdir. It uses the solver variables stored in the
lockdir when filtering dependencies which have predicates such as ":with-doc".
If the with-doc variable wasn't stored in the lockdir then this command would
fail as the locked dependency "foo" would appear extraneous.
  $ dune describe pkg list-locked-dependencies
  Dependencies of local packages locked in dune.lock
  - Immediate dependencies of local package x.dev
    - foo.0.0.1
    
