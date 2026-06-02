Test private virtual libraries and implementations

  $ cat > dune-project <<EOF
  > (lang dune 3.13)
  > (using melange 0.1)
  > EOF
  $ make_melange_virtual_time_project "" "" timeJs

  $ dune build @melange
