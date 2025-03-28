Nix can leave a symlink to a store path in the tree, often called 'result'.
'dune fmt' crashes because of that.

  $ RESULT=`mktemp -d`
  $ echo "let x  = 2" > "$RESULT/foo.ml"
  $ chmod -R a-w "$RESULT"
  $ ln -s "$RESULT" result

This command should succeed:

  $ dune fmt
  File "ocamlformat.ml", line 1, characters 0-0:
  Error: Files _build/default/ocamlformat.ml and
  _build/default/.formatted/ocamlformat.ml differ.
  File "result/foo.ml", line 1, characters 0-0:
  Error: Files _build/default/result/foo.ml and
  _build/default/result/.formatted/foo.ml differ.
  Promoting _build/default/.formatted/ocamlformat.ml to ocamlformat.ml.
  Promoting _build/default/result/.formatted/foo.ml to result/foo.ml.
  Error: failed to promote result/foo.ml
  Permission denied
  [1]

Allow Dune to remove temporary files (calling Dune crashes without this):

  $ chmod -R u+w "$RESULT"
