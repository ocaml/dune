Nix can leave a symlink to a store path in the tree, often called 'result'.
'dune fmt' crashes because of that.

  $ RESULT=`mktemp -d`
  $ echo "let x  = 2" > "$RESULT/foo.ml"
  $ chmod -R a-w "$RESULT"
  $ ln -s "$RESULT" result

This command should succeed:

  $ dune fmt
  File "ocamlformat.ml", line 1, characters 0-0:
  --- ocamlformat.ml
  +++ .formatted/ocamlformat.ml
  @@ -1,2 +1 @@
  -(* Avoid adding dependencies to this cram test *)
  -let () = print_endline "(* formatted *)"
  +(* formatted *)
  File "result/foo.ml", line 1, characters 0-0:
  --- result/foo.ml
  +++ result/.formatted/foo.ml
  @@ -1 +1 @@
  -let x  = 2
  +(* formatted *)
  Promoting _build/default/.formatted/ocamlformat.ml to ocamlformat.ml.
  Promoting _build/default/result/.formatted/foo.ml to result/foo.ml.
  Error: Error promoting _build/.promotion-staging/result/foo.ml to
  result/foo.ml
  Unix.Unix_error(Unix.EACCES, "unlink", "result/foo.ml")
  [1]

Allow Dune to remove temporary files (calling Dune crashes without this):

  $ chmod -R u+w "$RESULT"
