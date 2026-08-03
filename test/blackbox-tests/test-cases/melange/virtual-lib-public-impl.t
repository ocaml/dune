Test virtual libraries where the virtual implementation is a public library

  $ cat > dune-project <<EOF
  > (lang dune 3.13)
  > (using melange 0.1)
  > (package (name the_lib))
  > (package (name concrete_lib))
  > EOF
  $ make_melange_virtual_time_project the_lib concrete_lib concrete_lib

  $ dune build @melange
