The presence of a directory whose name is not a valid module name should not
clash with (include_subdirs qualified) if a parent of the directory in question
is included in a (data_only_dirs ...) stanza.

  $ dune exec ./foo.exe
  baaar
