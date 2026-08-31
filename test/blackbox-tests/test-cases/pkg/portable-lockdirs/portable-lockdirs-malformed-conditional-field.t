Malformed conditional fields in a new-format lock directory must report the
conditional grammar error. They must not be retried as legacy bare fields.

  $ mkdir -p "${source_lock_dir}"
  $ cat >"${source_lock_dir}/lock.dune" <<'EOF'
  > (lang package 0.1)
  > (repositories (complete true))
  > (solved_for_platforms
  >  ((arch x86_64)
  >   (os linux)))
  > EOF
  $ make_lockpkg foo.0.0.1 <<'EOF'
  > (version 0.0.1)
  > (depends
  >  (choice not-a-conditional))
  > EOF

  $ build_pkg foo
  File "_build/_private/default/.lock/dune.lock/foo.0.0.1.pkg", line 3, characters 9-26:
  3 |  (choice not-a-conditional))
               ^^^^^^^^^^^^^^^^^
  Error: List expected
  [1]
