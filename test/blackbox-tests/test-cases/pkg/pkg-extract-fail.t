Note that test output here is heavily sanitized due to tools using different
exit codes and error messages, see #11560 for example output

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
  Solution for .dune-solution-cache:
  - foo.0.0.1
  $ build_pkg foo 2>&1 |  sed -ne '/Error:/,$ p' | sed '/^Reason/ q' | sed "s/'[0-9]*'/X/"
  Error: failed to extract 'corrupted.tar'
  Reason: 'tar' failed with non-zero exit code X and output:

Repeat the same test as above but ensure that error output from gzip is
captured

  $ echo "corrupted tar.gz" > corrupted.tar.gz

  $ mkpkg foo <<EOF
  > url {
  >  src: "corrupted.tar.gz"
  > }
  > EOF

  $ solve foo
  Solution for .dune-solution-cache:
  - foo.0.0.1

  $ build_pkg foo 2>&1 |  sed -ne '/Error:/,$ p' | sed '/^Reason/ q' | sed "s/'[0-9]*'/X/"
  Error: failed to extract 'corrupted.tar.gz'
  Reason: 'tar' failed with non-zero exit code X and output:

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
  Solution for .dune-solution-cache:
  - foo.0.0.1

  $ build_pkg foo 2>&1 | sed -ne '/Error:/,$ p' | sed '/^Reason/ q' | sed "s/'[0-9]*'/X/"
  Error: failed to extract 'corrupted.zip'
  Reason: 'unzip' failed with non-zero exit code X and output:
