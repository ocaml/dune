Test dune rules

  $ cat > dune-project <<EOF
  > (lang dune 3.8)
  > (using melange 0.1)
  > EOF

  $ cat > dune <<EOF
  > (melange.emit
  >  (target output)
  >  (modules main))
  > EOF

  $ cat > main.ml <<EOF
  > Js.log "hello"
  > EOF

  $ dune build
  Creating manifest rule
    dir: _build/default
    target_dir: _build/default/output
    mappings count: 1
    manifest path: _build/default/output/melange-manifest.sexp
    manifest content:
  ((_build/default/main.ml (_build/default/output/main.js)))
  $ dune rules @all | grep -C 3 "manifest"

  $ ls _build/default/output
  main.js
  node_modules
