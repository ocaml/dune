  $ echo "(lang dune 2.0)" > dune-project
  $ mkdir src
  $ ln -s src src-clone
  $ dune build
  Error: Path src has already been scanned. Cannot scan it again through
  symlink src-clone
  [1]
