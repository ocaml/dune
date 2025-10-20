We test how opam files with depopts fields are translated into dune.lock files:

  $ . ../helpers.sh
  $ mkrepo

Make a package with a depopts field
  $ mkpkg with-depopts <<'EOF'
  > depopts: [ "foo" ]
  > EOF
  $ mkpkg foo

  $ solve with-depopts
  Solution for dune.lock:
  - with-depopts.0.0.1

  $ promote() {
  >  cp -r "${default_lock_dir}" "${source_lock_dir}"
  > }


When depopts are supported and selected, the above lock should change and we
should also be able to see a deps field in the lock file:

  $ cat ${default_lock_dir}/with-depopts.pkg
  (version 0.0.1)

We should also be able to validate the lock directory:

  $ promote
  $ dune pkg validate-lockdir

Depopts should not be selected if they conflict with other constraints:

  $ mkpkg no-foo <<'EOF'
  > depends: [ "with-depopts" ]
  > conflicts: [ "foo" ]
  > EOF

  $ rm -r "${source_lock_dir}"
  $ solve no-foo
  Solution for dune.lock:
  - no-foo.0.0.1
  - with-depopts.0.0.1

  $ promote
  $ dune pkg validate-lockdir
