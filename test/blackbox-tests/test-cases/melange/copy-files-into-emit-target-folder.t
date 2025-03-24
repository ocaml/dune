Example showing melange.emit and copy_files, where the files are copied
into the melange.emit target folder

  $ cat > dune-project <<EOF
  > (lang dune 3.8)
  > (using melange 0.1)
  > EOF

  $ mkdir src public

  $ cat > src/dune <<EOF
  > (melange.emit
  >  (target output)
  >  (emit_stdlib false)
  >  (alias mel))
  > EOF

  $ touch public/img.png

Builds normally

  $ dune build @mel

Now try copying a file

  $ cat > src/dune <<EOF
  > (melange.emit
  >  (target output)
  >  (emit_stdlib false)
  >  (alias mel))
  > 
  > (subdir
  >  output
  >  (subdir
  >   src
  >   (copy_files
  >    (files %{project_root}/public/img.png))))
  > EOF

It works:

  $ dune build @mel

We add a module and it stays working:

  $ cat > src/a.ml <<EOF
  > let () = Js.log "foo"
  > EOF

  $ dune build @mel
