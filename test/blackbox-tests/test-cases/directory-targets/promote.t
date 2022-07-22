Promotion of directory targets.

  $ mkdir test; cd test
  $ cat > dune-project <<EOF
  > (lang dune 3.0)
  > (using directory-targets 0.1)
  > EOF
  $ cat > dune <<EOF
  > (rule
  >   (mode promote)
  >   (deps (sandbox always))
  >   (targets a (dir dir))
  >   (action (bash "\| echo a > a;
  >                 "\| mkdir -p dir/subdir;
  >                 "\| echo b > dir/b;
  >                 "\| echo c > dir/c;
  >                 "\| echo d > dir/subdir/d
  > )))
  > EOF

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

Promoting a badly specified directory target gives a weird error:

  $ cat > dune-project <<EOF
  > (lang dune 3.2)
  > (using directory-targets 0.1)
  > EOF

  $ cat > dune <<EOF
  > (rule
  >  (targets blah-blah)
  >  (deps (sandbox always))
  >  (mode promote)
  >  (action (bash "mkdir %{targets}")))
  > EOF

  $ dune build
  Error: Is a directory
  -> required by _build/default/blah-blah
  -> required by alias all
  -> required by alias default
  [1]

Test error message for (promote (into <dir>)) if <dir> is missing.

  $ cat > dune <<EOF
  > (rule
  >   (mode (promote (into another_dir)))
  >   (deps (sandbox always))
  >   (targets a (dir dir))
  >   (action (bash "\| echo a > a;
  >                 "\| mkdir -p dir/subdir;
  >                 "\| echo b > dir/b;
  >                 "\| echo c > dir/c;
  >                 "\| echo d > dir/subdir/d
  > )))
  > EOF

  $ dune build a
  File "dune", line 2, characters 23-34:
  2 |   (mode (promote (into another_dir)))
                             ^^^^^^^^^^^
  Error: Directory "another_dir" does not exist. Please create it manually.
  -> required by _build/default/a
  [1]

Test cleaning up unexpected files and directories in directory targets.

  $ cat > dune <<EOF
  > (rule
  >   (mode (promote))
  >   (deps (sandbox always))
  >   (targets a (dir dir))
  >   (action (bash "\| echo a > a;
  >                 "\| mkdir -p dir/subdir;
  >                 "\| echo b > dir/b;
  >                 "\| echo c > dir/c;
  >                 "\| echo d > dir/subdir/d
  > )))
  > EOF

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
