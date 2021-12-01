  $ export BUILD_PATH_PREFIX_MAP=odoc=`command -v odoc`

As configured in the `dune` file at the root, this should be an error:

  $ dune build --only-packages=foo_doc @doc
  File "../../../../foo_doc/foo.mld", line 4, characters 0-0:
  Error: End of text is not allowed in '[...]' (code).
  ERROR: Warnings have been generated.
  [1]

Same for documentation in mli files:

  $ dune build --only-packages=foo_lib @doc
  File "foo_lib/foo.mli", line 1, characters 7-7:
  Error: End of text is not allowed in '[...]' (code).
  ERROR: Warnings have been generated.
  [1]

These packages are in a nested env, the option is disabled, should success with warning printed:

  $ dune build --only-packages=bar_doc,bar_lib @doc
  File "../../../../sub_env/bar_doc/bar.mld", line 4, characters 0-0:
  Warning: End of text is not allowed in '[...]' (code).
  File "sub_env/bar_lib/bar.mli", line 1, characters 7-7:
  Warning: End of text is not allowed in '[...]' (code).

In release mode, no error:

  $ dune build -p foo_doc,foo_lib @doc
  (cd _build/default/_doc/_odoc/pkg/foo_doc && odoc compile --pkg foo_doc -o page-foo.odoc ../../../../foo_doc/foo.mld)
  File "../../../../foo_doc/foo.mld", line 4, characters 0-0:
  Warning: End of text is not allowed in '[...]' (code).
  (cd _build/default/foo_lib/.foo.objs/byte && odoc compile -I . -I ../../../_doc/_odoc/pkg/foo_lib --pkg foo_lib -o foo.odoc foo.cmti)
  File "foo_lib/foo.mli", line 1, characters 7-7:
  Warning: End of text is not allowed in '[...]' (code).
