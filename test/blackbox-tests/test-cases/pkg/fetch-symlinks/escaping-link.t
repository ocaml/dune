Test that we don't allow symlinks to escape the project root.

Case 1: standard relative directory link
  $ mkdir _outside_sources/
  $ echo "secret" > _outside_sources/file.txt

  $ mkdir -p _src/
  $ echo "content" > _src/file.txt
  $ ln -s ../_outside_sources _src/link_to_outside

  $ make_lockdir

  $ tar czf _src.tar.gz _src

Foo tests if the package is succesfully extracted and built
  $ make_lockpkg foo <<EOF
  > (version 0.0.1)
  > (source
  >  (fetch
  >   (url file://$PWD/_src.tar.gz)))
  > (build (run cat file.txt))
  > EOF

Bar tests if the symlink is still usable after extraction
  $ make_lockpkg bar <<EOF
  > (version 0.0.1)
  > (source
  >  (fetch
  >   (url file://$PWD/_src.tar.gz)))
  > (build (run cat link_to_outside/file.txt))
  > EOF

This works because since the link was relative, when fetched it becomes invalid.
We then delete it silently.
  $ build_pkg foo
  content

  $ dune trace cat | jq 'select(.args.message == "Deleted broken symlink from fetched archive") | {args}' | sanitize_pkg_digest foo.0.0.1
  {
    "args": {
      "message": "Deleted broken symlink from fetched archive",
      "full_name": "_build/_private/default/.pkg/foo.0.0.1-DIGEST_HASH/source/link_to_outside"
    }
  }

This should fail since the symlink was deleted
  $ build_pkg bar
  File "dune.lock/bar.pkg", line 5, characters 12-15:
  5 | (build (run cat link_to_outside/file.txt))
                  ^^^
  Error: Logs for package bar
  cat: link_to_outside/file.txt: No such file or directory
  
  [1]

Case 2 relative directory link that is still valid after extraction

  $ rm _src/link_to_outside
  $ ln -s ../../../../../../_outside_sources _src/link_to_outside

  $ tar czf _src.tar.gz _src

This fails correctly, the link isn't allowed to go outside
  $ build_pkg foo 2>&1 | sanitize_pkg_digest foo.0.0.1 | tail -3
  Error: Unable to resolve symlink
  _build/_private/default/.pkg/foo.0.0.1-DIGEST_HASH/source/link_to_outside:
  its target _outside_sources is outside the source directory
  [1]

Case 3: absolute directory link

  $ rm _src/link_to_outside
  $ echo "secret" > $PWD/_outside_sources/file.txt
  $ ln -s $PWD/_outside_sources _src/link_to_outside

  $ tar czf _src.tar.gz _src

This fails correctly
  $ build_pkg foo 2>&1 | sanitize_pkg_digest foo.0.0.1 | tail -5
  Error: Unable to resolve symlink
  _build/_private/default/.pkg/foo.0.0.1-DIGEST_HASH/source/link_to_outside:
  its target
  $TESTCASE_ROOT/_outside_sources
  is outside the source directory
  [1]

Case 4: relative file links

  $ rm _src/link_to_outside
  $ ln -s ../_outside_sources/file.txt _src/link_to_secret

  $ tar czf _src.tar.gz _src

  $ make_lockpkg bar <<EOF
  > (version 0.0.1)
  > (source
  >  (fetch
  >   (url file://$PWD/_src.tar.gz)))
  > (build (run cat link_to_secret))
  > EOF

This should either work and print 'content' or fail???
  $ build_pkg foo

TODO: I don't even know what should happen here
  $ build_pkg bar
  File "dune.lock/bar.pkg", line 5, characters 12-15:
  5 | (build (run cat link_to_secret))
                  ^^^
  Error: Logs for package bar
  cat: link_to_secret: No such file or directory
  
  [1]

Case 5: absolute file links
  $ rm _src/link_to_secret
  $ ln -s $PWD/_outside_sources/file.txt _src/link_to_secret

  $ tar czf _src.tar.gz _src

This fails correctly
  $ build_pkg foo 2>&1 | sanitize_pkg_digest foo.0.0.1 | tail -5
  Error: Unable to resolve symlink
  _build/_private/default/.pkg/foo.0.0.1-DIGEST_HASH/source/link_to_secret:
  its target
  $TESTCASE_ROOT/_outside_sources/file.txt
  is outside the source directory
  [1]
