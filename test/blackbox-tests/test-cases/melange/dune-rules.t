Test dune rules

  $ cat > dune-project <<EOF
  > (lang dune 3.8)
  > (using melange 0.1)
  > EOF

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

  $ dune rules @melange | grep In_build_dir
      (In_build_dir _build/default/.output.mobjs/melange/melange__Main.cmj))))

Using output folder fails

  $ dune rules output
  Error: Don't know how to build output
  [1]

Creating dir fixes the problem

  $ mkdir output

  $ dune rules output | grep In_build_dir
      (In_build_dir _build/default/.output.mobjs/melange/melange__Main.cmj))))
