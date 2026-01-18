Report an error when absolute paths appear in the install stanza

  $ cat >dune-project <<EOF
  > (lang dune 3.6)
  > (package (name foo))
  > EOF

Put $PWD in a file that can be read with the %{read:...} pform, so the underline
in the error message is of consistent length on different systems.
  $ printf $PWD > pwd

  $ touch foo.txt

  $ cat >dune <<EOF
  > (install
  >  (files %{read:pwd}/foo.txt)
  >  (section share))
  > EOF

  $ dune build @install
  File "dune", line 2, characters 8-27:
  2 |  (files %{read:pwd}/foo.txt)
              ^^^^^^^^^^^^^^^^^^^
  Error: Absolute paths are not allowed in the install stanza.
  [1]

  $ mkdir -p bar
  $ touch bar/bar.txt

  $ cat >dune <<EOF
  > (install
  >  (dirs %{read:pwd}/bar)
  >  (section share))
  > EOF

  $ dune build @install
  File "dune", line 2, characters 7-22:
  2 |  (dirs %{read:pwd}/bar)
             ^^^^^^^^^^^^^^^
  Error: Absolute paths are not allowed in the install stanza.
  [1]
