Test case for https://github.com/ocaml/dune/issues/7573

This test case demonstrates build failures given the existence of intentially
broken symbollic links, even in data_only_dirs.

  $ cat > dune-project << EOF
  > (lang dune 3.8)
  > EOF

  $ mkdir foo
  $ cat > dune << EOF
  > (data_only_dirs foo)
  > EOF

  $ (cd foo && ln -s doesnt_exist bar)

  $ dune build foo/bar --debug-digest --debug-cache=shared,workspace-local,fs
  Workspace-local cache miss: _build/default/.dune/configurator: never seen this target before
  Shared cache miss [464c107d149be4e276c1b307136d2461] (_build/default/.dune/configurator): cache disabled
  Workspace-local cache miss: _build/default/.dune/configurator.v2: never seen this target before
  Shared cache miss [39cdfe356740bbb5db762f5afad29bb5] (_build/default/.dune/configurator.v2): cache disabled
  File "foo/bar", line 1, characters 0-0:
  Error: File unavailable: foo/bar
  peek_or_refresh_file:
   - bs: true
   - path: In_source_tree "dune-project"
  refresh_without_removing case
  result: Ok digest "7ddb0f449bf0945e9b4c83281633c698"
  peek_or_refresh_file:
   - bs: true
   - path: In_source_tree "dune"
  refresh_without_removing case
  result: Ok digest "e8b8642beda7ac787266c4d773cfb4b5"
  peek_or_refresh_file:
   - bs: true
   - path: In_source_tree "foo/bar"
  refresh_without_removing case
  here
  exn: Unix.Unix_error(Unix.ENOENT, "open", "foo/bar")
  result: No_such_file
  [1]
