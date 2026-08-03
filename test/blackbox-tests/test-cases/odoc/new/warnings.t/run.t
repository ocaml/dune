  $ export BUILD_PATH_PREFIX_MAP=odoc=`command -v odoc`
  $ foo_doc_odoc=_build/default/_doc_new/odoc/local/foo_doc/page-foo.odoc
  $ foo_lib_odoc=_build/default/_doc_new/odoc/local/foo_lib/foo.odoc
  $ bar_doc_odoc=_build/default/_doc_new/odoc/local/bar_doc/page-bar.odoc
  $ bar_lib_odoc=_build/default/_doc_new/odoc/local/bar_lib/bar.odoc

As configured in the `dune` file at the root, this should be an error:

  $ dune build --only-packages=foo_doc "$foo_doc_odoc"
  File "../../../../foo_doc/foo.mld", line 4, characters 0-0:
  Error: End of text is not allowed in '[...]' (code).
  ERROR: Warnings have been generated.
  [1]

Same for documentation in mli files:

  $ dune build --only-packages=foo_lib "$foo_lib_odoc"
  File "foo_lib/foo.mli", line 1, characters 7-7:
  Error: End of text is not allowed in '[...]' (code).
  ERROR: Warnings have been generated.
  [1]

These packages are in a nested env, the option is disabled, should success with warning printed:

  $ dune build --only-packages=bar_doc,bar_lib \
  >   "$bar_doc_odoc" \
  >   "$bar_lib_odoc"
  File "../../../../sub_env/bar_doc/bar.mld", line 4, characters 0-0:
  Error: End of text is not allowed in '[...]' (code).
  ERROR: Warnings have been generated.
  File "sub_env/bar_lib/bar.mli", line 1, characters 7-7:
  Error: End of text is not allowed in '[...]' (code).
  ERROR: Warnings have been generated.
  [1]

In release mode, no error:

  $ dune build -p foo_doc,foo_lib \
  >   "$foo_doc_odoc" \
  >   "$foo_lib_odoc" 2>&1 | censor
  (cd _build/.sandbox/$DIGEST1/default/_doc_new/odoc/local/foo_doc && odoc compile -o page-foo.odoc ../../../../foo_doc/foo.mld -I ../../../index/local/foo_doc --parent 'page-"foo_doc"')
  File "../../../../foo_doc/foo.mld", line 4, characters 0-0:
  Warning: End of text is not allowed in '[...]' (code).
  (cd _build/.sandbox/$DIGEST2/default/_doc_new/odoc/local/foo_lib && odoc compile -I . -I ../../stdlib -o foo.odoc ../../../../foo_lib/.foo.objs/byte/foo.cmti -I ../../../index/local/foo_lib --parent 'page-"foo_lib"')
  File "foo_lib/foo.mli", line 1, characters 7-7:
  Warning: End of text is not allowed in '[...]' (code).
