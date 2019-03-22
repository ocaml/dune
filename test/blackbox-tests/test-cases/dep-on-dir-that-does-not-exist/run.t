# Here we test that actions depending on a glob correctly
# depend on the directory existence as well.
#
# Currently we do something slightly buggy though.
#
# One of these behaviors would be reasonable instead:
# 
# - refuse to run an action if the directory is missing (for a well-
# defined definition of "missing")
# - mkdir the directory unconditionally before running the action
# - track the dependency and rerun it when the directory is created.

  $ dune build @print-contents-of-dir-that-is-later-created
  File "dune", line 8, characters 22-53:
  8 |  (deps    (glob_files dir-that-is-later-created/*.txt))
                            ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
  Warning: Directory dir-that-is-later-created doesn't exist.
  listing: ERROR

^ Reasonable

  $ mkdir dir-that-is-later-created
  $ echo '(alias (name foo) (deps) (action (echo "")))' > dir-that-is-later-created/dune

  $ rm -r _build; dune build @print-contents-of-dir-that-is-later-created
  listing: ERROR

^ Strange: didn't we create the directory?

  $ rm -r _build
  $ dune build @dir-that-is-later-created/foo
  $ dune build @print-contents-of-dir-that-is-later-created
  listing: 

^ Wat: unrelated alias caused the listing to succeed.

  $ rm -r _build
  $ dune build @print-contents-of-dir-that-is-later-created
  listing: ERROR

^ Yeah, clearly a bug: the output depends on whether or not the unrelated
alias is forced.

  $ rm -r _build
  $ dune build @print-contents-of-dir-that-is-later-created
  listing: ERROR
  $ dune build @dir-that-is-later-created/foo
  $ dune build @print-contents-of-dir-that-is-later-created
  listing: ERROR

^ Also a bug: the value is not recomputed after the directory is created.

  $ touch dir-that-is-later-created/blah.txt
  $ dune build @print-contents-of-dir-that-is-later-created
  listing: blah.txt

^ Ok, if there are some files matching the pattern, then it becomes good.
