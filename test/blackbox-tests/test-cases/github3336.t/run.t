Issue #3336 describes a bug where it's not possible to use dune_build_info from
ppx binaries.

  $ dune exec ./executable/exec.exe 2>&1 | grep -v "^File" | sed -E 's/from .+/from ../'
  Error: No implementations provided for the following modules:
           Build_info__Build_info_data referenced from ..
