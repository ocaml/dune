We test how opam files with depopts fields are translated into .dune-solution-cache files:

  $ . ../helpers.sh
  $ mkrepo

Make a package with a depopts field
  $ mkpkg with-depopts <<'EOF'
  > depopts: [ "foo" ]
  > EOF
  $ mkpkg foo

  $ solve with-depopts
  Solution for .dune-solution-cache:
  - with-depopts.0.0.1

When depopts are supported and selected, the above lock should change and we
should also be able to see a deps field in the lock file:

  $ cat ${default_lock_dir}/with-depopts.pkg
  (version 0.0.1)

We should also be able to validate the lock directory:

  $ dune pkg validate-lockdir

Depopts should not be selected if they conflict with other constraints:

  $ mkpkg no-foo <<'EOF'
  > depends: [ "with-depopts" ]
  > conflicts: [ "foo" ]
  > EOF

  $ solve no-foo
  Solution for .dune-solution-cache:
  - no-foo.0.0.1
  - with-depopts.0.0.1

  $ dune pkg validate-lockdir
