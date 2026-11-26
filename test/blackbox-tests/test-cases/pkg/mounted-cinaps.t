A cinaps stanza in a project mounted from a lock directory passes the matching
files of its mounted directory to cinaps (cinaps/simple.t).

  $ make_lockdir
  $ make_named_package_project main 3.25 foo
  $ mkdir _source
  $ (cd _source && make_cinaps_project 3.25 1.0)
  $ echo '(package (name foo))' >> _source/dune-project
  $ echo '(cinaps (files *.ml))' > _source/dune
  $ cat > _source/test.ml <<'EOF2'
  > (*$ print_endline "\nhello" *)
  > (*$*)
  > let x = 1
  > EOF2
  $ echo 'not cinapsed' > _source/other.txt
  $ make_dune_source_lockpkg native foo "$PWD/_source"

The mount only sees libraries from the lock directory, so the cinaps runtime
library is not found and the cinaps executable cannot be built.

  $ dune build @_build/default/.pkg/foo/cinaps 2>&1 | grep '^Error'
  Error: Library "cinaps.runtime" not found.
  [1]

The staged generator was still produced from the mounted test.ml.

  $ grep -o "[a-z_]*[.]ml\b" _build/default/.pkg/foo/.cinaps.*/cinaps.ml-gen | sort -u
  test.ml
