The presence of a directory whose name is not a valid module name should not
clash with (include_subdirs qualified) if the directory in question is ignored
by a (dirs :standard \ ...) stanza.

  $ dune exec ./foo.exe
  baaar
