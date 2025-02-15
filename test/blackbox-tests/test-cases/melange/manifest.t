Test melange manifest

  $ cat > dune-project <<EOF
  > (lang dune 3.8)
  > (using melange 0.1)
  > EOF

  $ cat > dune <<EOF
  > (melange.emit
  >  (target output)
  >  (emit_manifest true)
  >  (modules main))
  > EOF

  $ cat > main.ml <<EOF
  > Js.log "hello"
  > EOF

  $ dune build @melange
  Creating manifest rule
    dir: _build/default
    target_dir: _build/default/output
    mappings count: 1
    manifest path: _build/default/output/melange-manifest.sexp
    manifest content:
  [{"_build/default/main.ml":["_build/default/output/main.js"]}]

  $ ls _build/default/output
  main.js
  melange-manifest.sexp
  node_modules

  $ cat _build/default/output/melange-manifest.sexp
  [{"_build/default/main.ml":["_build/default/output/main.js"]}]

  $ dune rules @melange | grep -C 3 "manifest"
  Creating manifest rule
    dir: _build/default
    target_dir: _build/default/output
    mappings count: 1
    manifest path: _build/default/output/melange-manifest.sexp
    manifest content:
  [{"_build/default/main.ml":["_build/default/output/main.js"]}]
  
  ((deps ())
   (targets
    ((files (_build/default/output/melange-manifest.sexp)) (directories ())))
   (context default)
   (action
    (chdir
     _build/default
     (write-file
      output/melange-manifest.sexp
      "[{\"_build/default/main.ml\":[\"_build/default/output/main.js\"]}]"))))
  
  ((deps
