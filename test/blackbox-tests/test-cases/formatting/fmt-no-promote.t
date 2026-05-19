  $ cat > dune-project << EOF
  > (lang dune 3.0)
  > EOF

Make a dune file that should be formatted.
  $ cat > dune << EOF
  > (rule (write-file a b))
  > EOF

Run `dune fmt --preview` twice to test it is idempotent. In a terminal these
commands would also print the diff of what would be changed.
  $ dune fmt --preview
  File "dune", line 1, characters 0-0:
  --- dune
  +++ dune.corrected
  @@ -1 +1,2 @@
  -(rule (write-file a b))
  +(rule
  + (write-file a b))
  [1]
  $ dune fmt --preview
  File "dune", line 1, characters 0-0:
  --- dune
  +++ dune.corrected
  @@ -1 +1,2 @@
  -(rule (write-file a b))
  +(rule
  + (write-file a b))
  [1]

No formatted file target is materialized under _build.
  $ test ! -e _build/default/.formatted/dune

Actually format the file
  $ dune fmt
  File "dune", line 1, characters 0-0:
  --- dune
  +++ dune.corrected
  @@ -1 +1,2 @@
  -(rule (write-file a b))
  +(rule
  + (write-file a b))
  Promoting _build/default/dune.corrected to dune.
  [1]

Now the output of `dune fmt --preview is empty`.
  $ dune fmt --preview
