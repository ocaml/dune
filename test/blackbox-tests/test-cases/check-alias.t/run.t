Check that the @check alias builds:
- the merlin file
- the cmi/cmo/cmt files

A test should be considered successful if it prints the .merlin files
as well as the foo.{cmi,cmo,cmt} files.

  $ build_check_and_list_interesting_files_in ()
  > (
  >   cd $1
  >   dune build @check
  >   find _build \( -name '*.cm*' -o -name '.merlin-conf' \) | awk -F/ '{ print $NF }' | LANG=C sort
  > )

Test the property for executables:

  $ build_check_and_list_interesting_files_in exe
  .merlin-conf
  foo.cmi
  foo.cmo
  foo.cmt

Test the property for libraries:

  $ build_check_and_list_interesting_files_in lib
  .merlin-conf
  foo.cmi
  foo.cmo
  foo.cmt
