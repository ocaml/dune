:standard in :include'd files is ignored (issue #13225)

When :standard is used inside an :include'd file, environment flags are not
applied.

  $ cat > dune-project <<EOF
  > (lang dune 3.18)
  > EOF

Set a custom flag in the environment:

  $ cat > dune <<EOF
  > (env
  >  (dev
  >   (flags (-w -8))))
  > (library
  >  (name direct)
  >  (modules direct)
  >  (flags (:standard)))
  > (library
  >  (name included)
  >  (modules included)
  >  (flags (:include flags.sexp)))
  > (rule
  >  (action (write-file flags.sexp "(:standard)")))
  > EOF

  $ cat > direct.ml <<EOF
  > let x = 1
  > EOF

  $ cat > included.ml <<EOF
  > let x = 1
  > EOF

  $ show_flags() {
  >   dune trace cat \
  >     | jq -r --arg f "$1" 'select(.args.process_args | last == $f) | .args.process_args | join(" ")'
  > }

Direct :standard correctly picks up the environment flag:

  $ dune build direct.cma
  $ show_flags direct.ml | grep -o '\-w -8'
  -w -8

But :include containing :standard silently ignores environment flags:

  $ dune build included.cma
  $ show_flags included.ml | grep -o '\-w -8' || echo "flag missing"
  flag missing
