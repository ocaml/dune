Test with C stubs in sub-directories
------------------------------------

  $ dune runtest
  File "dune", line 9, characters 16-25:
  9 |  (c_names stub1 sub/stub2))
                      ^^^^^^^^^
  Error: Relative part of stub is not necessary and should be removed. To
  include sources in subdirectories, use the (include_subdirs ...) stanza.
  [1]

