Includes may ascend to a parent directory inside a mount, even above a nested
project root. They must not leave either the logical mount or its backing tree.

  $ make_lockdir
  $ make_dune_project 3.25
  $ mkdir -p _source/project/sub
  $ echo '(lang dune 3.25)' > _source/dune-project
  $ (cd _source/project && make_dune_project_with_package 3.25 foo)
  $ echo '(include ../../shared.inc)' > _source/project/sub/dune
  $ cat > _source/shared.inc <<'EOF'
  > (rule (target result) (action (write-file %{target} original)))
  > EOF
  $ cat > _source/dune <<'EOF'
  > (rule (target generated.inc) (action (copy shared.inc %{target})))
  > EOF

The same parent include works in the workspace. Mounting uses the prepared
version of the included file, without modifying the origin.

  $ (cd _source && dune build --root . project/sub/result)
  $ cat _source/_build/default/project/sub/result
  original
  $ make_dune_source_lockpkg native foo "$PWD/_source"
  $ sed s/original/prepared/ _source/shared.inc | make_lockpkg_file foo shared.inc
  $ result=_build/default/.pkg/foo/project/sub/result
  $ build_result() {
  >   rm -f "$result"
  >   dune build "$result" && cat "$result" && return
  >   s=$?; test ! -e "$result" || echo "$result exists"; return $s
  > }
  $ build_result
  prepared
  $ cat _source/shared.inc
  (rule (target result) (action (write-file %{target} original)))

A missing parent include inside the mount still has its ordinary diagnostic.

  $ echo '(include ../../missing.inc)' > _source/project/sub/dune
  $ build_result
  File "_build/default/.pkg/foo/project/sub/dune", line 1, characters 0-27:
  Error: File _build/default/.pkg/foo/missing.inc doesn't exist.
  [1]

Returning to the backing directory after ascending past its root cannot make
an include escape the logical mount, even if the backing file exists.

  $ echo '(include ../../../source/shared.inc)' > _source/project/sub/dune
  $ build_result
  File "_build/default/.pkg/foo/project/sub/dune", line 1, characters 0-36:
  Error: Included path "./../../../source/shared.inc" is outside the mounted
  source tree.
  [1]

Conversely, returning to the logical mount after ascending past its root can
leave the backing directory. This must be rejected before testing existence.

  $ echo '(include ../../../foo/missing.inc)' > _source/project/sub/dune
  $ build_result
  File "_build/default/.pkg/foo/project/sub/dune", line 1, characters 0-34:
  Error: Included path "./../../../foo/missing.inc" is outside the mounted
  source tree.
  [1]

Includes emitted by an OCaml-syntax dune file have the same parent access and
boundary checks.

  $ sed -i s/prepared/script-prepared/ dune.lock/foo.files/shared.inc
  $ cat > _source/project/sub/dune <<'EOF'
  > (* -*- tuareg -*- *)
  > let () = Jbuild_plugin.V1.send "(include ../../shared.inc)"
  > EOF
  $ build_result
  script-prepared
  $ sed -i 's#\.\./\.\./#../../../source/#' _source/project/sub/dune
  $ build_result
  File "_build/.dune/default/.pkg/foo/project/sub/dune", line 1, characters 0-36:
  1 | (include ../../../source/shared.inc)
      ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
  Error: Included path "../../../source/shared.inc" is outside the mounted
  source tree.
  [1]

Generated includes may also reach a parent inside the mount. The generated file
is built in the mount rather than read directly from the backing directory.

  $ sed -i s/script-/dynamic-/ dune.lock/foo.files/shared.inc
  $ echo '(dynamic_include ../../generated.inc)' > _source/project/sub/dune
  $ (cd _source && dune build --root . project/sub/result)
  $ cat _source/_build/default/project/sub/result
  original
  $ build_result
  dynamic-prepared

A dynamic include cannot load a workspace file outside the mount.

  $ sed s/original/workspace/ _source/shared.inc > outside.inc
  $ echo '(dynamic_include ../../../../outside.inc)' > _source/project/sub/dune
  $ build_result
  File "_build/default/.pkg/foo/project/sub/dune", line 1, characters 0-41:
  Error: Included path "../../../../outside.inc" is outside the mounted source
  tree.
  [1]

The boundary is retained when a generated include loads another include.

  $ echo '(dynamic_include ../../generated.inc)' > _source/project/sub/dune
  $ echo '(dynamic_include ../../outside.inc)' > dune.lock/foo.files/shared.inc
  $ build_result
  File "_build/default/.pkg/foo/generated.inc", line 1, characters 0-35:
  1 | (dynamic_include ../../outside.inc)
      ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
  Error: Included path "../../outside.inc" is outside the mounted source tree.
  [1]

Include-loop diagnostics continue to use the logical paths inside the mount.

  $ echo '(include ../../shared.inc)' > _source/project/sub/dune
  $ echo '(include project/sub/dune)' > dune.lock/foo.files/shared.inc
  $ build_result
  File "_build/default/.pkg/foo/project/sub/dune", line 1, characters 0-26:
  Error: Recursive inclusion of dune files detected:
  File _build/default/.pkg/foo/project/sub/dune is included from
  _build/default/.pkg/foo/shared.inc:1
     included from _build/default/.pkg/foo/project/sub/dune:1
  [1]

The same boundaries apply when no overlay requires a prepared directory.

  $ rm dune.lock/foo.files/shared.inc
  $ build_result
  original
  $ echo '(include ../../../source/shared.inc)' > _source/project/sub/dune
  $ build_result
  File "_build/default/.pkg/foo/project/sub/dune", line 1, characters 0-36:
  Error: Included path "./../../../source/shared.inc" is outside the mounted
  source tree.
  [1]
