Test private virtual libraries and implementations

  $ make_melange_project 3.13 0.1
  $ make_melange_virtual_time_project "" "" timeJs

  $ dune build @melange
