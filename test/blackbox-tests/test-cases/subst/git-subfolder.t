Running `dune subst` in a subfolder of a Git repository should work.
Regression test for https://github.com/ocaml/dune/issues/11045

  $ git init --quiet
  $ dune init proj subfolder
  Entering directory 'subfolder'
  Success: initialized project component named subfolder
  Leaving directory 'subfolder'

  $ unset INSIDE_DUNE

`dune subst` requires at least one commit in the repository.

  $ git add -A && git commit --quiet --message "Initial commit"
  $ cd subfolder
  $ dune subst
  Raised when trying to print location { pos_fname = "subfolder.opam"
  ; start = { pos_lnum = 1; pos_bol = 0; pos_cnum = 10 }
  ; stop = { pos_lnum = 1; pos_bol = 0; pos_cnum = 25 }
  }
  /-----------------------------------------------------------------------
  | Internal error: Uncaught exception.
  | End_of_file
  | Raised at Stdlib.input_line.scan in file "stdlib.ml", line 456, characters 14-31
  | Called from Stdune__Exn.protectx in file "otherlibs/stdune/src/exn.ml", line 10, characters 8-11
  | Re-raised at Stdune__Exn.protectx in file "otherlibs/stdune/src/exn.ml", line 16, characters 4-11
  | Called from Stdune__Result.try_with in file "otherlibs/stdune/src/result.ml", line 30, characters 8-12
  \-----------------------------------------------------------------------
  
  File "subfolder.opam", line 1, characters 10-25:
  Error: repository does not contain any version information
  [1]
