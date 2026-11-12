An unreadable regular source must stop installation before later entries are
copied. Make the source unreadable after building the package so the failure
occurs during installation, not during the build.

  $ make_dune_project_with_package 3.25 foo
  $ printf 'first\n' >first
  $ printf 'second\n' >second
  $ cat >dune <<'EOF'
  > (install
  >  (section share)
  >  (files first second))
  > EOF
  $ dune build @install
  $ chmod 000 _build/default/first

Users who can still read mode-000 files, such as root, cannot exercise this
permission failure.

  $ if test ! -r _build/default/first; then
  >   dune install --prefix prefix >install.log 2>&1
  >   status=$?
  >   test "$status" -eq 1 &&
  >   grep -q 'Permission denied' install.log &&
  >   test ! -e prefix/share/foo/second
  > fi
  $ chmod u+rw _build/default/first
