c_names with overlapping names in different stanzas

  $ dune build @all 2>&1 | dune_cmd sanitize
  File "dune", line 4, characters 10-13:
  4 |  (c_names foo))
                ^^^
  Error: Multiple definitions for the same object file "foo$ext_obj". See another
  definition at dune:9.
  Hint: You can avoid the name clash by renaming one of the objects, or by
  placing it into a different directory.
