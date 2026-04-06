Test the "dune internal digest-db" command.

  $ echo '(lang dune 3.0)' > dune-project
  $ mkdir -p dir/sub
  $ printf x > invalid.txt
  $ printf y > stale.txt
  $ printf a > dir/a
  $ printf b > dir/sub/b
  $ touch -t 200001010000 invalid.txt
  $ dune build invalid.txt stale.txt dir/a dir/sub/b

The full dump includes cached entries:

  $ dune internal digest-db dump 2>&1 | grep -o 'In_source_tree "invalid.txt"'
  In_source_tree "invalid.txt"

Dumping a file only shows its exact entry:

  $ dune internal digest-db dump invalid.txt 2>&1 | grep -o 'In_source_tree "invalid.txt"'
  In_source_tree "invalid.txt"
  $ dune internal digest-db dump invalid.txt 2>&1 | grep 'In_source_tree "stale.txt"'
  [1]

Dumping a directory only shows its direct children:

  $ dune internal digest-db dump dir 2>&1 | grep -o 'In_source_tree "dir/a"'
  In_source_tree "dir/a"
  $ dune internal digest-db dump dir 2>&1 | grep 'In_source_tree "dir/sub/b"'
  [1]

Modify one file without changing the cached stats and another one normally.

  $ printf z > invalid.txt
  $ touch -t 200001010000 invalid.txt
  $ printf yz > stale.txt

Only actual digest mismatches are reported, classified as invalid or stale.

  $ dune internal digest-db check invalid.txt stale.txt 2>&1 \
  > | sed -n 's/.*status = /status = /p;s/.*path = /path = /p'
  status = "invalid"
  path = In_source_tree "invalid.txt"
  status = "stale"
  path = In_source_tree "stale.txt"
