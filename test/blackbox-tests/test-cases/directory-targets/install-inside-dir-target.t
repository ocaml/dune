Observe `(install (dirs ...))` and `(install (files ...))` when the path
refers to something inside a directory target. Four cases:


  $ cat >dune-project <<EOF
  > (lang dune 3.24)
  > (package (name p))
  > EOF

A rule that produces a directory target containing a file and a
sub-directory with a file inside it:

  $ cat >setup.dune <<EOF
  > (rule
  >  (target (dir gen))
  >  (action
  >   (progn (run mkdir -p %{target}/sub)
  >          (run touch %{target}/top.txt)
  >          (run touch %{target}/sub/inside.txt))))
  > EOF

(dirs ...) of a file inside the directory target:

  $ cat setup.dune >dune
  $ cat >>dune <<EOF
  > (install (section share) (dirs gen/top.txt))
  > EOF
  $ dune build @install
  File "dune", line 7, characters 31-42:
  7 | (install (section share) (dirs gen/top.txt))
                                     ^^^^^^^^^^^
  Error: _build/default/gen/top.txt is a file, not a directory.
  Hint: Use (install (files ...)) for files.
  [1]

(files ...) of a file inside the directory target:

  $ cat setup.dune >dune
  $ cat >>dune <<EOF
  > (install (section share) (files gen/top.txt))
  > EOF
  $ dune build @install

(files ...) of a sub-directory inside the directory target:

  $ cat setup.dune >dune
  $ cat >>dune <<EOF
  > (install (section share) (files gen/sub))
  > EOF
  $ dune build @install
  File "dune", line 7, characters 32-39:
  7 | (install (section share) (files gen/sub))
                                      ^^^^^^^
  Error: _build/default/gen/sub is a directory, not a file.
  Hint: Use (install (dirs ...)) for directory targets.
  [1]

(dirs ...) of a sub-directory inside the directory target:

  $ cat setup.dune >dune
  $ cat >>dune <<EOF
  > (install (section share) (dirs gen/sub))
  > EOF
  $ dune build @install
