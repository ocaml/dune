  $ cat >dune-project <<EOF
  > (lang dune 3.11)
  > (package (name x))
  > EOF

Make some hidden folders containing text files.
  $ mkdir -p a/.b/c
  $ mkdir -p a/d/.e
  $ echo "hello" > a/.b/c/foo.txt
  $ echo "world" > a/d/.e/bar.txt

Rules that copy the text files:
  $ cat >dune <<EOF
  > (rule
  >  (deps (source_tree a))
  >  (target baz.txt)
  >  (action
  >   (run cp a/.b/c/foo.txt baz.txt)))
  > 
  > (rule
  >  (deps (source_tree a))
  >  (target qux.txt)
  >  (action
  >   (run cp a/d/.e/bar.txt qux.txt)))
  > EOF

Thse will fail fail because dune doesn't copy hidden directories by default.
  $ dune build baz.txt
  File "dune", line 2, characters 20-21:
  2 |  (deps (source_tree a))
                          ^
  Warning: The source directory a has a subdirectory a/.b which was not copied
  to the build directory and will not be visible to build actions.
  Hint: This could be because the directory is empty or due to dune's rules for
  excluding directories. By default directories whose names begin with '.' or
  '_' are excluded. This can be prevented by adding a `(dirs ...)` stanza to
  a/dune which explicitly includes the directory a/.b.
  File "dune", line 2, characters 20-21:
  2 |  (deps (source_tree a))
                          ^
  Warning: The source directory a has a subdirectory a/.b/c which was not
  copied to the build directory and will not be visible to build actions.
  Hint: This could be because the directory is empty or due to dune's rules for
  excluding directories. By default directories whose names begin with '.' or
  '_' are excluded. This can be prevented by adding a `(dirs ...)` stanza to
  a/.b/dune which explicitly includes the directory a/.b/c.
  File "dune", line 2, characters 20-21:
  2 |  (deps (source_tree a))
                          ^
  Warning: The source directory a has a subdirectory a/d which was not copied
  to the build directory and will not be visible to build actions.
  Hint: This could be because the directory is empty or due to dune's rules for
  excluding directories. By default directories whose names begin with '.' or
  '_' are excluded. This can be prevented by adding a `(dirs ...)` stanza to
  a/dune which explicitly includes the directory a/d.
  File "dune", line 2, characters 20-21:
  2 |  (deps (source_tree a))
                          ^
  Warning: The source directory a has a subdirectory a/d/.e which was not
  copied to the build directory and will not be visible to build actions.
  Hint: This could be because the directory is empty or due to dune's rules for
  excluding directories. By default directories whose names begin with '.' or
  '_' are excluded. This can be prevented by adding a `(dirs ...)` stanza to
  a/d/dune which explicitly includes the directory a/d/.e.
  File "dune", line 1, characters 0-92:
  1 | (rule
  2 |  (deps (source_tree a))
  3 |  (target baz.txt)
  4 |  (action
  5 |   (run cp a/.b/c/foo.txt baz.txt)))
  cp: cannot stat 'a/.b/c/foo.txt': No such file or directory
  [1]
  $ dune build qux.txt
  File "dune", line 8, characters 20-21:
  8 |  (deps (source_tree a))
                          ^
  Warning: The source directory a has a subdirectory a/.b which was not copied
  to the build directory and will not be visible to build actions.
  Hint: This could be because the directory is empty or due to dune's rules for
  excluding directories. By default directories whose names begin with '.' or
  '_' are excluded. This can be prevented by adding a `(dirs ...)` stanza to
  a/dune which explicitly includes the directory a/.b.
  File "dune", line 8, characters 20-21:
  8 |  (deps (source_tree a))
                          ^
  Warning: The source directory a has a subdirectory a/.b/c which was not
  copied to the build directory and will not be visible to build actions.
  Hint: This could be because the directory is empty or due to dune's rules for
  excluding directories. By default directories whose names begin with '.' or
  '_' are excluded. This can be prevented by adding a `(dirs ...)` stanza to
  a/.b/dune which explicitly includes the directory a/.b/c.
  File "dune", line 8, characters 20-21:
  8 |  (deps (source_tree a))
                          ^
  Warning: The source directory a has a subdirectory a/d which was not copied
  to the build directory and will not be visible to build actions.
  Hint: This could be because the directory is empty or due to dune's rules for
  excluding directories. By default directories whose names begin with '.' or
  '_' are excluded. This can be prevented by adding a `(dirs ...)` stanza to
  a/dune which explicitly includes the directory a/d.
  File "dune", line 8, characters 20-21:
  8 |  (deps (source_tree a))
                          ^
  Warning: The source directory a has a subdirectory a/d/.e which was not
  copied to the build directory and will not be visible to build actions.
  Hint: This could be because the directory is empty or due to dune's rules for
  excluding directories. By default directories whose names begin with '.' or
  '_' are excluded. This can be prevented by adding a `(dirs ...)` stanza to
  a/d/dune which explicitly includes the directory a/d/.e.
  File "dune", line 7, characters 0-92:
   7 | (rule
   8 |  (deps (source_tree a))
   9 |  (target qux.txt)
  10 |  (action
  11 |   (run cp a/d/.e/bar.txt qux.txt)))
  cp: cannot stat 'a/d/.e/bar.txt': No such file or directory
  [1]

This dune file forces dune to copy a/.b
  $ cat >a/dune <<EOF
  > (dirs :standard .b)
  > EOF

Now the first rule will succeed but the second rule will still fail
  $ dune build baz.txt
  File "dune", line 2, characters 20-21:
  2 |  (deps (source_tree a))
                          ^
  Warning: The source directory a has a subdirectory a/d which was not copied
  to the build directory and will not be visible to build actions.
  Hint: This could be because the directory is empty or due to dune's rules for
  excluding directories. By default directories whose names begin with '.' or
  '_' are excluded. This can be prevented by adding a `(dirs ...)` stanza to
  a/dune which explicitly includes the directory a/d.
  File "dune", line 2, characters 20-21:
  2 |  (deps (source_tree a))
                          ^
  Warning: The source directory a has a subdirectory a/d/.e which was not
  copied to the build directory and will not be visible to build actions.
  Hint: This could be because the directory is empty or due to dune's rules for
  excluding directories. By default directories whose names begin with '.' or
  '_' are excluded. This can be prevented by adding a `(dirs ...)` stanza to
  a/d/dune which explicitly includes the directory a/d/.e.
  $ dune build qux.txt
  File "dune", line 8, characters 20-21:
  8 |  (deps (source_tree a))
                          ^
  Warning: The source directory a has a subdirectory a/d which was not copied
  to the build directory and will not be visible to build actions.
  Hint: This could be because the directory is empty or due to dune's rules for
  excluding directories. By default directories whose names begin with '.' or
  '_' are excluded. This can be prevented by adding a `(dirs ...)` stanza to
  a/dune which explicitly includes the directory a/d.
  File "dune", line 8, characters 20-21:
  8 |  (deps (source_tree a))
                          ^
  Warning: The source directory a has a subdirectory a/d/.e which was not
  copied to the build directory and will not be visible to build actions.
  Hint: This could be because the directory is empty or due to dune's rules for
  excluding directories. By default directories whose names begin with '.' or
  '_' are excluded. This can be prevented by adding a `(dirs ...)` stanza to
  a/d/dune which explicitly includes the directory a/d/.e.
  File "dune", line 7, characters 0-92:
   7 | (rule
   8 |  (deps (source_tree a))
   9 |  (target qux.txt)
  10 |  (action
  11 |   (run cp a/d/.e/bar.txt qux.txt)))
  cp: cannot stat 'a/d/.e/bar.txt': No such file or directory
  [1]

This dune file forces dune to copy a/d/.e
  $ cat >a/d/dune <<EOF
  > (dirs :standard .e)
  > EOF

Now both rules will succeed.
  $ dune build baz.txt
  $ dune build qux.txt
