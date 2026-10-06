Promotion of directory targets.

  $ mkdir test; cd test
  $ make_directory_targets_project 3.0
  $ write_promoted_directory_target_rule() {
  > local mode="$1"
  > cat > dune <<EOF
  > (rule
  >  (mode ${mode})
  >  (deps (sandbox always))
  >  (targets a (dir dir))
  >  (action (bash "\| echo a > a;
  >                "\| mkdir -p dir/subdir;
  >                "\| echo b > dir/b;
  >                "\| echo c > dir/c;
  >                "\| echo d > dir/subdir/d
  > )))
  > EOF
  > }
  $ write_promoted_directory_target_rule promote

  $ dune build a
  $ cat a dir/b dir/c dir/subdir/d
  a
  b
  c
  d

If a destination directory is taken up by a file, Dune deletes it.

  $ rm -rf dir
  $ mkdir dir
  $ touch dir/subdir
  $ dune build a
  $ cat a dir/b dir/c dir/subdir/d
  a
  b
  c
  d

If a destination file is taken up by a directory, Dune deletes it.

  $ rm dir/b
  $ mkdir -p dir/b
  $ touch dir/b
  $ dune build a
  $ cat a dir/b dir/c dir/subdir/d
  a
  b
  c
  d

A directory declared as a file target is rejected before promotion:

  $ make_directory_targets_project 3.2

  $ cat > dune <<EOF
  > (rule
  >  (targets blah-blah)
  >  (deps (sandbox always))
  >  (mode promote)
  >  (action (bash "mkdir %{targets}")))
  > EOF

  $ dune build
  File "dune", lines 1-5, characters 0-104:
  1 | (rule
  2 |  (targets blah-blah)
  3 |  (deps (sandbox always))
  4 |  (mode promote)
  5 |  (action (bash "mkdir %{targets}")))
  Error: Error trying to read targets after a rule was run:
  - blah-blah: Directory produced for a file target. Use (dir blah-blah).
  [1]

Test error message for (promote (into <dir>)) if <dir> is missing.

  $ write_promoted_directory_target_rule "(promote (into another_dir))"

  $ dune build a

Test cleaning up unexpected files and directories in directory targets.

  $ write_promoted_directory_target_rule "(promote)"

  $ mkdir -p dir/unexpected-dir-1
  $ mkdir -p dir/subdir/unexpected-dir-2
  $ touch dir/unexpected-file-1
  $ touch dir/unexpected-dir-1/unexpected-file-2
  $ touch dir/subdir/unexpected-file-3
  $ dune build a

  $ ls dir | grep unexpected
  [1]
  $ ls dir/subdir | grep unexpected
  [1]
