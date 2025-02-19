Nix can leave a symlink to a store path in the tree, often called 'result'.
'dune fmt' crashes because of that.

  $ RESULT=`mktemp -d`
  $ echo "let x  = 2" > "$RESULT/foo.ml"
  $ chmod -R a-w "$RESULT"
  $ ln -s "$RESULT" result

Formatting 'foo.ml' shouldn't be attempted and this command should succeed:

  $ dune fmt

Allow Dune to remove temporary files (calling Dune crashes without this):

  $ chmod -R u+w "$RESULT"
