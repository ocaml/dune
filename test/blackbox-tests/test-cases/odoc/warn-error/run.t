Show commands run by dune, output is reproducible

  $ cat <<"EOF" > dune_verbose.sh
  > dune "$@" --display verbose 2>&1 | \
  > grep -v -e '^# ' -e '^Running\[.*\]: .*/ocamlc.opt -config > /tmp/dune.*\.output$' | \
  > sed "s#$(opam var prefix)/#%OPAM_PREFIX%/#"
  > EOF

All calls to `odoc compile` and `odoc html` should have the `--warn-error` option.

  $ . ./dune_verbose.sh build @doc
  Running[1]: (cd _build/default && %OPAM_PREFIX%/bin/ocamldep.opt -modules -intf lib/foo.mli) > _build/default/lib/.foo.objs/foo.mli.d
  Running[2]: (cd _build/default && %OPAM_PREFIX%/bin/ocamlc.opt -w @1..3@5..28@30..39@43@46..47@49..57@61..62-40 -strict-sequence -strict-formats -short-paths -keep-locs -g -bin-annot -I lib/.foo.objs/byte -no-alias-deps -opaque -o lib/.foo.objs/byte/foo.cmi -c -intf lib/foo.mli)
  Running[3]: (cd _build/default/_doc/_odoc/pkg/foo && %OPAM_PREFIX%/bin/odoc compile --warn-error --pkg foo -o page-foo.odoc ../../../../doc/foo.mld)
  Running[4]: (cd _build/default/_doc/_odoc/pkg/foo && %OPAM_PREFIX%/bin/odoc compile --warn-error --pkg foo -o page-index.odoc ../../../_mlds/foo/index.mld)
  Running[5]: (cd _build/default/lib/.foo.objs/byte && %OPAM_PREFIX%/bin/odoc compile --warn-error -I . -I ../../../_doc/_odoc/pkg/foo --pkg foo -o foo.odoc foo.cmti)
  Running[6]: (cd _build/default/_doc/_html && %OPAM_PREFIX%/bin/odoc html --warn-error -I ../_odoc/pkg/foo -I ../../lib/.foo.objs/byte -o . ../_odoc/pkg/foo/page-foo.odoc)
  Running[7]: (cd _build/default && %OPAM_PREFIX%/bin/odoc support-files -o _doc/_html)
  Running[8]: (cd _build/default/_doc/_html && %OPAM_PREFIX%/bin/odoc html --warn-error -I ../_odoc/pkg/foo -I ../../lib/.foo.objs/byte -o . ../_odoc/pkg/foo/page-index.odoc)
  Running[9]: (cd _build/default/_doc/_html && %OPAM_PREFIX%/bin/odoc html --warn-error -I ../_odoc/pkg/foo -I ../../lib/.foo.objs/byte -o . ../../lib/.foo.objs/byte/foo.odoc)
