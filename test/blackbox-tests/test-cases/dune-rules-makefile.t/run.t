Testing the makefile generated by dune rules --makefile

  $ dune rules --makefile > Makefile

  $ make _build/default/simple_exec.exe > /dev/null

  $ _build/default/simple_exec.exe
  Hello, world!
