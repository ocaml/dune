Test dune rules

  $ cat > dune-project <<EOF
  > (lang dune 3.8)
  > (using melange 0.1)
  > EOF

Using flags field in melange.emit stanzas is not supported

  $ cat > dune <<EOF
  > (melange.emit
  >  (target output)
  >  (emit_stdlib false)
  >  (modules main))
  > EOF

  $ cat > main.ml <<EOF
  > print_endline "hello"
  > EOF

Calling dune rules with the alias works fine

  $ dune rules @melange
  ((deps
    ((File (External /home/me/code/dune/_opam/bin/melc))
     (File
      (In_build_dir _build/default/.output.mobjs/melange/melange__Main.cmj))))
   (targets ((files (default/output/main.js)) (directories ())))
   (context default)
   (action
    (chdir
     _build/default
     (run
      /home/me/code/dune/_opam/bin/melc
      -I
      .output.mobjs/melange
      --bs-module-type
      commonjs
      -o
      output/main.js
      .output.mobjs/melange/melange__Main.cmj))))

Using output folder fails

  $ dune rules output
  Error: Don't know how to build output
  [1]

Creating dir fixes the problem

  $ mkdir output

  $ dune rules output
  ((deps
    ((File (External /home/me/code/dune/_opam/bin/melc))
     (File
      (In_build_dir _build/default/.output.mobjs/melange/melange__Main.cmj))))
   (targets ((files (default/output/main.js)) (directories ())))
   (context default)
   (action
    (chdir
     _build/default
     (run
      /home/me/code/dune/_opam/bin/melc
      -I
      .output.mobjs/melange
      --bs-module-type
      commonjs
      -o
      output/main.js
      .output.mobjs/melange/melange__Main.cmj))))
