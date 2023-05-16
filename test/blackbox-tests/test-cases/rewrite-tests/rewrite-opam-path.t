Test the rewriting of paths in the opam subtree.

  $ cat > dune-project <<EOF
  > (lang dune 3.7)
  > EOF

  $ cat > dune <<EOF
  > (rule
  >  (target x)
  >  (action
  >    (system "dune_cmd rewrite-path $OPAM_SWITCH_PREFIX;touch x"
  >    )))
  > (rule
  >  (target y)
  >  (action
  >    (system "dune_cmd rewrite-path `which ocamlc`;touch y"
  >    )))
  > EOF

  $ dune build --display short @all 2>&2 | dune_cmd sanitize
            sh x
  /workspace_root
            sh y
  /workspace_root/bin/ocamlc
