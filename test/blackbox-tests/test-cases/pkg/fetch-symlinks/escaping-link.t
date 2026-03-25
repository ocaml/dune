Test that we don't allow symlinks to escape the project root.

Case 1: relative directory link
  $ mkdir _outside_sources/
  $ echo "secret" > _outside_sources/file.txt

  $ mkdir -p _src/
  $ echo "content" > _src/file.txt
  $ ln -s ../_outside_sources _src/link_to_outside

  $ make_lockdir

  $ make_lockpkg foo <<EOF
  > (version 0.0.1)
  > (source
  >  (fetch
  >   (url file://$PWD/_src)))
  > (build (run cat file.txt))
  > EOF

  $ tar czf _src.tar.gz _src

  $ make_lockpkg bar <<EOF
  > (version 0.0.1)
  > (source
  >  (fetch
  >   (url file://$PWD/_src.tar.gz)))
  > (build (run cat file.txt))
  > EOF

This error is correct, but not the one expected
  $ build_pkg foo
  Error: path outside the workspace: ../_outside_sources from .
  [1]

This works because since the link was relative, when fetched it becomes invalid.
We then delete it silently.
  $ build_pkg bar
  content

  $ dune trace cat | jq 'select(.args.message == "Deleted broken symlink from fetched archive") | {args}' | sanitize_pkg_digest bar.0.0.1
  {
    "args": {
      "message": "Deleted broken symlink from fetched archive",
      "path": "_build/_private/default/.pkg/bar.0.0.1-DIGEST_HASH/source/link_to_outside"
    }
  }

Case 2: absolute directory link

  $ rm _src/link_to_outside
  $ ln -s $PWD/_outside_sources _src/link_to_outside

  $ tar czf _src.tar.gz _src

This fails correctly, although the formatting is a bit messy
  $ build_pkg foo 2>&1 | sed -E 's#/.*.sandbox/[^/]+#/.sandbox/$SANDBOX#g'
  File "_build/_private/default/.lock/dune.lock/foo.pkg", line 4, characters 7-150:
  4 |   (url file:/.sandbox/$SANDBOX/default/test/blackbox-tests/test-cases/pkg/fetch-symlinks/_src)))
             ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
  Error: Unable to resolve symlink
  /.sandbox/$SANDBOX/default/test/blackbox-tests/test-cases/pkg/fetch-symlinks/_src/link_to_outside:
  its target
  /.sandbox/$SANDBOX/default/test/blackbox-tests/test-cases/pkg/fetch-symlinks/_src
  is outside the source directory
  [1]

This also fails correctly
  $ build_pkg bar 2>&1 | sed -E 's#/.*.sandbox/[^/]+#/.sandbox/$SANDBOX#g' | sanitize_pkg_digest bar.0.0.1
  File "_build/_private/default/.lock/dune.lock/bar.pkg", line 4, characters 7-157:
  4 |   (url file:/.sandbox/$SANDBOX/default/test/blackbox-tests/test-cases/pkg/fetch-symlinks/_src.tar.gz)))
             ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
  Error: Unable to resolve symlink
  _build/_private/default/.pkg/bar.0.0.1-DIGEST_HASH/source/link_to_outside:
  its target
  _build/_private/default/.pkg/bar.0.0.1-DIGEST_HASH/source
  is outside the source directory
  [1]

Case 3: relative file links

  $ rm _src/link_to_outside
  $ ln -s ../_outside_sources/file.txt _src/link_to_secret

  $ tar czf _src.tar.gz _src

This should fail
  $ build_pkg foo
  content

This should also fail
  $ build_pkg bar

Case 4: absolute file links
  $ rm _src/link_to_secret
  $ ln -s $PWD/_outside_sources/file.txt _src/link_to_secret

  $ tar czf _src.tar.gz _src

This fails correctly, although the formatting is a bit messy
  $ build_pkg foo  2>&1 | sed -E 's#/.*.sandbox/[^/]+#/.sandbox/$SANDBOX#g'
  File "_build/_private/default/.lock/dune.lock/foo.pkg", line 4, characters 7-150:
  4 |   (url file:/.sandbox/$SANDBOX/default/test/blackbox-tests/test-cases/pkg/fetch-symlinks/_src)))
             ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
  Error: Unable to resolve symlink
  /.sandbox/$SANDBOX/default/test/blackbox-tests/test-cases/pkg/fetch-symlinks/_src/link_to_secret:
  its target
  /.sandbox/$SANDBOX/default/test/blackbox-tests/test-cases/pkg/fetch-symlinks/_src
  is outside the source directory
  [1]

This also fails correctly
  $ build_pkg bar 2>&1 | sed -E 's#/.*.sandbox/[^/]+#/.sandbox/$SANDBOX#g' | sanitize_pkg_digest bar.0.0.1
  File "_build/_private/default/.lock/dune.lock/bar.pkg", line 4, characters 7-157:
  4 |   (url file:/.sandbox/$SANDBOX/default/test/blackbox-tests/test-cases/pkg/fetch-symlinks/_src.tar.gz)))
             ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
  Error: Unable to resolve symlink
  _build/_private/default/.pkg/bar.0.0.1-DIGEST_HASH/source/link_to_secret:
  its target
  _build/_private/default/.pkg/bar.0.0.1-DIGEST_HASH/source
  is outside the source directory
  [1]
