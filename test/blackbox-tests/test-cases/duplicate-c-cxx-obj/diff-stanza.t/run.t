This test showcases that although libraries can technically have non overlapping
stubs names, things are still broken if their .o files overlap:

  $ dune build @all 2>&1 | dune_cmd sanitize
  File "dune", line 4, characters 10-13:
  4 |  (c_names foo))
                ^^^
  Error: Multiple definitions for the same object file "foo$ext_obj". See another
  definition at dune:9.
  Hint: You can avoid the name clash by renaming one of the objects, or by
  placing it into a different directory.
