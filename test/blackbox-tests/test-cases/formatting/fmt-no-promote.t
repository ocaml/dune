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
  Error: Files _build/default/dune and _build/default/.formatted/dune differ.
  [1]
  $ dune fmt --preview
  File "dune", line 1, characters 0-0:
  Error: Files _build/default/dune and _build/default/.formatted/dune differ.
  [1]

Show the formatted file from _build.
  $ cat _build/default/.formatted/dune
  (rule
   (write-file a b))

Actually format the file
  $ dune fmt
  File "dune", line 1, characters 0-0:
  Error: Files _build/default/dune and _build/default/.formatted/dune differ.
  Promoting _build/default/.formatted/dune to dune.
  [1]

Now the output of `dune fmt --preview is empty`.
  $ dune fmt --preview
