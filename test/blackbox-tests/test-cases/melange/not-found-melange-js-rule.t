Demonstrates some issue while trying to copy files into melange target folder

  $ cat > dune-project <<EOF
  > (lang dune 3.7)
  > (using melange 0.1)
  > EOF

  $ mkdir src public

  $ cat > src/dune <<EOF
  > (melange.emit
  >  (target output)
  >  (alias melange)
  >  (module_system commonjs))
  > EOF

  $ touch public/img.png

Builds normally

  $ dune build @melange

Now try copying a file

  $ cat > src/dune <<EOF
  > (melange.emit
  >  (target output)
  >  (alias melange)
  >  (module_system commonjs))
  > 
  > (subdir
  >  output
  >  (subdir
  >   src
  >   (copy_files
  >    (files %{project_root}/public/img.png))))
  > EOF

Build fails

  $ dune build @melange
  Error: No rule found for src/output/src/.output.mobjs/melange.js
  -> required by alias src/melange
  [1]

Adding a module doesn't help

  $ cat > src/a.ml <<EOF
  > let () = Js.log "foo"
  > EOF

  $ dune build @melange
  Error: No rule found for src/output/src/a.js
  -> required by alias src/melange
  [1]
