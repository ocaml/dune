A mounted package whose source is a local directory uses that directory as
its backing, without first copying a snapshot into _build/_fetch.

  $ make_foo_consumer
  $ mkdir -p _source/sub _source/empty _source/_build _source/.git
  $ (cd _source && make_dune_project_with_package 3.25 foo)
  $ printf '(library (public_name foo))\n(include stanzas.inc)\n' > _source/dune
  $ echo '(rule (target included) (action (write-file %{target} included)))' > _source/stanzas.inc
  $ echo 'let message = "original"' > _source/foo.ml
  $ echo '(rule (target out) (action (write-file %{target} sub)))' > _source/sub/dune
  $ mkdir _shared
  $ printf '#!/bin/sh\necho executable\n' > _shared/tool.sh
  $ chmod +x _shared/tool.sh
  $ ln -s ../_shared/tool.sh _source/tool.sh
  $ echo junk > _source/_build/junk.txt
  $ echo junk > _source/.git/config
  $ echo junk > '_source/.#editor.ml'
  $ make_dune_source_lockpkg native foo "$PWD/_source"

Building a consumer does not need a snapshot of the source under _build/_fetch.

  $ dune build main.exe
  $ _build/default/main.exe
  original
  $ test ! -d _build/_fetch

Editing a file on disk rebuilds the consumer.

  $ echo 'let message = "edited"' > _source/foo.ml
  $ dune build main.exe
  $ _build/default/main.exe
  edited

Adding and deleting files in a subdirectory are picked up.

  $ dune build _build/default/.pkg/foo/sub/out
  $ cat _build/default/.pkg/foo/sub/out
  sub
  $ rm _source/sub/dune
  $ dune build _build/default/.pkg/foo/sub/out 2>&1 | head -2
  Error: Don't know how to build _build/default/.pkg/foo/sub/out
  [1]
  $ echo 'let message = Extra.message' > _source/foo.ml
  $ echo 'let message = "added"' > _source/extra.ml
  $ dune build main.exe
  $ _build/default/main.exe
  added
  $ rm _source/extra.ml
  $ dune build main.exe 2>&1 | grep -c 'Unbound module'
  1
  [1]
  $ echo 'let message = "edited"' > _source/foo.ml

VCS and build directories and editor lock files of the source are not mounted.

  $ dune build main.exe
  $ test ! -e _build/default/.pkg/foo/_build
  $ test ! -e _build/default/.pkg/foo/.git
  $ dune build '_build/default/.pkg/foo/.#editor.ml' 2>&1 | head -1
  Error: Don't know how to build _build/default/.pkg/foo/.#editor.ml
  [1]

An executable script keeps its mode in the mount.

  $ dune build _build/default/.pkg/foo/tool.sh
  $ _build/default/.pkg/foo/tool.sh
  executable
  $ test ! -L _build/default/.pkg/foo/tool.sh
  $ printf '#!/bin/sh\necho executable-edited\n' > _shared/tool.sh
  $ dune build _build/default/.pkg/foo/tool.sh
  $ _build/default/.pkg/foo/tool.sh
  executable-edited

Opaque recipes use the same direct backing. File links may point outside the
package source; their contents are copied, not the links. Recipe writes stay
inside the writable sandbox and do not change either origin.

  $ mkdir _recipe && cd _recipe && make_dune_project 3.25 && make_lockdir
  $ mkdir _source _shared
  $ echo original > _source/input.txt
  $ echo shared > _shared/input.txt
  $ ln -s ../_shared/input.txt _source/alias.txt
  $ make_lockpkg recipe <<EOF2
  > (version 1.0)
  > (source (copy $PWD/_source))
  > (build
  >  (progn
  >   (run cat input.txt alias.txt)
  >   (system "echo overwritten > input.txt")
  >   (write-file result built)
  >   (write-file recipe.install "share: [ \"result\" ]")))
  > EOF2
  $ dune build _build/default/.pkg/recipe/_install/share/recipe/result
  original
  shared
  $ cat _source/input.txt _shared/input.txt
  original
  shared
  $ test ! -L _build/default/.pkg/recipe/alias.txt
  $ test ! -d _build/_fetch

Editing either local input rebuilds the recipe without changing its sources.

  $ echo edited > _source/input.txt
  $ echo shared-edited > _shared/input.txt
  $ dune build _build/default/.pkg/recipe/_install/share/recipe/result
  edited
  shared-edited
  $ cat _source/input.txt _shared/input.txt
  edited
  shared-edited

A directory link must not become a route for reading source includes outside
an external backing. Only file links retain the main copy semantics.

  $ mkdir _include_links && cd _include_links && make_dune_project 3.25 && make_lockdir
  $ mkdir _source _outside
  $ (cd _source && make_dune_project_with_package 3.25 foo)
  $ echo '(include link/secret.inc)' > _source/dune
  $ echo '(rule (target escaped) (action (write-file %{target} outside)))' > _outside/secret.inc
  $ ln -s ../_outside _source/link
  $ make_dune_source_lockpkg native foo "$PWD/_source"
  $ dune build _build/default/.pkg/foo/escaped 2>&1
  File "_build/default/.pkg/foo/dune", line 1, characters 0-25:
  Error: File _build/default/.pkg/foo/link/secret.inc doesn't exist.
  [1]
  $ test ! -e _build/default/.pkg/foo/escaped

A synthetic subdirectory must not make that directory link traversable either.

  $ echo '(subdir link (include secret.inc))' > _source/dune
  $ dune build _build/default/.pkg/foo/link/escaped 2>&1
  File "_build/default/.pkg/foo/dune", line 1, characters 13-33:
  Error: File _build/default/.pkg/foo/link/secret.inc doesn't exist.
  [1]
  $ test ! -e _build/default/.pkg/foo/link/escaped

Repeated separators and parent components must select the same file in the
logical mount and the external backing, without reading through the link.

  $ echo '(rule (target normalized) (action (write-file %{target} inside)))' > _source/secret.inc
  $ echo '(include link//../secret.inc)' > _source/dune
  $ dune build _build/default/.pkg/foo/normalized
  $ cat _build/default/.pkg/foo/normalized
  inside
  $ test ! -e _build/default/.pkg/foo/escaped

The opaque inventory must not follow a directory link on a direct lookup.

  $ rm _source/dune
  $ dune build _build/default/.pkg/foo/link/secret.inc 2>&1
  Error: Don't know how to build _build/default/.pkg/foo/link/secret.inc
  [1]
