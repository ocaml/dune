Test runtime_deps field

  $ cat > dune-project <<EOF
  > (lang dune 3.7)
  > (using melange 0.1)
  > EOF

  $ cat > dune <<EOF
  > (melange.emit
  >  (target output)
  >  (entries main)
  >  (libraries lib)
  >  (module_system commonjs))
  > 
  > (library
  >  (name lib)
  >  (modules lib)
  >  (modes melange)
  >  (melange.runtime_deps assets/file.txt))
  > EOF

  $ cat > lib.ml <<EOF
  > let dirname = [%bs.raw "__dirname"]
  > let file_path = "assets/file.txt"
  > let file_content = Node.Fs.readFileSync (dirname ^ "/" ^ file_path) \`utf8
  > let print_file () = Js.log file_content
  > EOF

  $ mkdir assets
  $ cat > assets/file.txt <<EOF
  > hello from file
  > EOF

  $ cat > main.ml <<EOF
  > let () = Lib.print_file ()
  > EOF

  $ dune rules output/main.js
  Error: Dependency cycle between:
     Computing directory contents of _build/default
  -> Evaluating predicate in directory _build/default/output/assets
  -> Computing directory contents of _build/default
  [1]

$ node _build/default/output/main.js
hello from file

Can use more glob functionality to copy assets

  $ touch assets/foo.png
  $ touch assets/bar.png

  $ cat > dune <<EOF
  > (melange.emit
  >  (target output)
  >  (entries main)
  >  (libraries lib)
  >  (module_system commonjs))
  > 
  > (library
  >  (name lib)
  >  (modules lib)
  >  (modes melange)
  >  (melange.runtime_deps assets/*.png assets/file.txt))
  > EOF

  $ dune build output/main.js
  File "dune", line 11, characters 36-51:
  11 |  (melange.runtime_deps assets/*.png assets/file.txt))
                                           ^^^^^^^^^^^^^^^
  Error: Too many argument for melange.runtime_deps
  [1]

There is an error when adding runtime_deps to a non-Melange library

  $ cat > dune <<EOF
  > (melange.emit
  >  (target output)
  >  (entries main)
  >  (libraries lib)
  >  (module_system commonjs))
  > 
  > (library
  >  (name lib)
  >  (modules lib)
  >  (melange.runtime_deps assets/file.txt))
  > EOF

  $ dune build output/main.js
  File "dune", line 10, characters 23-38:
  10 |  (melange.runtime_deps assets/file.txt))
                              ^^^^^^^^^^^^^^^
  Error: Library "lib" is using the field melange.runtime_deps, but it is not a
  Melange library.
  Hint: Add "melange" to modes field
  Hint: Remove melange.runtime_deps
  [1]
