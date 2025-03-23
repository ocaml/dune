Create a mock package whose url is a corrupted/invalid tar file attempt to
build this package and check for sufficient error handling

  $ . ./helpers.sh
  $ echo "corrupted tar" > corrupted.tar

  $ mkpkg foo <<EOF
  > url {
  >  src: "corrupted.tar"
  > }
  > EOF

  $ add_mock_repo_if_needed
  $ solve foo
  Solution for dune.lock:
  - foo.0.0.1

  $ build_pkg foo 2>&1 | sed "s#\^##g; s#$(pwd)#PWD#; s#, characters.*##" | awk 'NF'
  File "dune.lock/foo.pkg", line 6
  6 |    file://PWD/corrupted.tar)))
  Error: failed to extract 'corrupted.tar'
  Reason: tar failed with non-zero exit code '2' and output:
  - /usr/bin/tar: This does not look like a tar archive
  - /usr/bin/tar: Exiting with failure status due to previous errors

Repeat the same test as above but ensure that error output from gzip is
captured

  $ rm -rd dune.lock
  $ echo "corrupted tar.gz" > corrupted.tar.gz

  $ mkpkg foo <<EOF
  > url {
  >  src: "corrupted.tar.gz"
  > }
  > EOF

  $ solve foo
  Solution for dune.lock:
  - foo.0.0.1

  $ build_pkg foo 2>&1 | sed "s#\^##g; s#$(pwd)#PWD#; s#, characters.*##" | awk 'NF'
  File "dune.lock/foo.pkg", line 6
  6 |    file://PWD/corrupted.tar.gz)))
  Error: failed to extract 'corrupted.tar.gz'
  Reason: tar failed with non-zero exit code '2' and output:
  - /usr/bin/tar: This does not look like a tar archive
  - 
  - gzip: stdin: not in gzip format
  - /usr/bin/tar: Child returned status 1
  - /usr/bin/tar: Error is not recoverable: exiting now
