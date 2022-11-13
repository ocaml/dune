Test the "dune internal dump" command.

  $ echo '(lang dune 3.0)' > dune-project
  $ cat > dune <<EOF
  > (rule
  >  (mode (promote (until-clean)))
  >  (action (with-stdout-to x (progn))))
  > EOF
  $ dune build x
  $ dune internal dump _build/.to-delete-in-source-tree
  set { "x" }
