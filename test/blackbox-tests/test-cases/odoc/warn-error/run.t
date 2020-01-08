Pass --warn-error to Odoc:

  $ cat <<EOF > dune-project
  > (lang dune 2.0)
  > (odoc_warn_error true)
  > EOF
  $ dune build @doc 2>&1 | grep -v "cd _build"
          odoc .foo.objs/byte/foo.odoc (exit 1)
  File "foo.mli", line 1, characters 12-12:
  End of text is not allowed in '[...]' (code).
  ERROR: Warnings have been generated.

Test without:

  $ echo "(lang dune 2.0)" > dune-project
  $ dune build @doc
          odoc .foo.objs/byte/foo.odoc
  File "foo.mli", line 1, characters 12-12:
  End of text is not allowed in '[...]' (code).
