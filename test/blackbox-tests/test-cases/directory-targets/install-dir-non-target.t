Observe what `(install (dirs ...))` does when the path is not a
directory target.

  $ cat >dune-project <<EOF
  > (lang dune 3.24)
  > (package (name p))
  > EOF

A plain source directory:

  $ mkdir -p assets
  $ echo "hello" > assets/file.txt
  $ cat >dune <<EOF
  > (install
  >  (section share)
  >  (dirs assets))
  > EOF
  $ dune build @install
  File "dune", line 3, characters 7-13:
  3 |  (dirs assets))
             ^^^^^^
  Error: No rule found for assets
  [1]

A plain file (not a directory):

  $ rm dune
  $ echo "data" > foo.txt
  $ cat >dune <<EOF
  > (install
  >  (section share)
  >  (dirs foo.txt))
  > EOF
  $ dune build @install
  File "dune", line 3, characters 7-14:
  3 |  (dirs foo.txt))
             ^^^^^^^
  Error: Rule produced unreadable directory "default/share/p/foo.txt"
  Not a directory
  [1]

A subdirectory of a directory target:

  $ rm dune foo.txt
  $ cat >dune <<EOF
  > (rule
  >  (target (dir gen))
  >  (action
  >   (progn (run mkdir -p %{target}/sub)
  >          (run touch %{target}/sub/inside.txt))))
  > (install
  >  (section share)
  >  (dirs gen/sub))
  > EOF
  $ dune build @install

