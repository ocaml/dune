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

Now try another local package but this time of zip format to test if stderr is
captured from the unzip tool. Note that preprocessing with sed here makes the
unzip error message a bit less clear

  $ echo "corrupted zip" > corrupted.zip

  $ mkpkg foo <<EOF
  > url {
  >  src: "corrupted.zip"
  > }
  > EOF

  $ add_mock_repo_if_needed
  $ solve foo
  Solution for dune.lock:
  - foo.0.0.1

  $ build_pkg foo 2>&1 | sed "s#\^##g; s#$(pwd)#PWD#; s#, characters.*##" | awk 'NF'
  File "dune.lock/foo.pkg", line 6
  6 |    file://PWD/corrupted.zip)))
  Error: failed to extract 'corrupted.zip'
  Reason: unzip failed with non-zero exit code '9' and output:
  -   End-of-central-directory signature not found.  Either this file is not
  -   a zipfile, or it constitutes one disk of a multi-part archive.  In the
  -   latter case the central directory and zipfile comment will be found on
  -   the last disk(s) of this archive.
  - unzip:  cannot find zipfile directory in one of
    PWD/corrupted.zip
    or
  -        
    PWD/corrupted.zip.zip,
    and cannot find
    PWD/corrupted.zip.ZIP,
    period.
