Test that extra files associated with a package are handled correctly when
multiple different versions of the package are present in the lockdir.

  $ mkrepo
  $ add_mock_repo_if_needed

Define 2 versions of the package foo that write their version number to a file
during their build so we can validate which version was built.

  $ VERSION1_FILE=mock-opam-repository/packages/foo/foo.1/files/version.txt
  $ VERSION2_FILE=mock-opam-repository/packages/foo/foo.2/files/version.txt

  $ mkdir -p $(dirname $VERSION1_FILE)
  $ echo version_1 > $VERSION1_FILE

  $ mkdir -p $(dirname $VERSION2_FILE)
  $ echo version_2 > $VERSION2_FILE

  $ mkpkg foo 1 <<EOF
  > build: [
  >   ["mkdir" "-p" share "%{lib}%/%{name}%"]
  >   ["touch" "%{lib}%/%{name}%/META"] # needed for dune to recognize this as a library
  > ]
  > extra-files: [
  >   ["version.txt" "md5=$(md5sum $VERSION1_FILE | cut -f1 -d' ')"]
  > ]
  > EOF
  $ mkpkg foo 2 <<EOF
  > build: [
  >   ["mkdir" "-p" share "%{lib}%/%{name}%"]
  >   ["touch" "%{lib}%/%{name}%/META"] # needed for dune to recognize this as a library
  > ]
  > extra-files: [
  >   ["version.txt" "md5=$(md5sum $VERSION2_FILE | cut -f1 -d' ')"]
  > ]
  > EOF

Define a package bar which conditionally depends on different versions of foo:

  $ make_platform_dependent_bar_package

Define a project with a package depending on bar:
  $ make_x_depends_bar_project

The platform-dependent version disagreement on foo is rejected by the
post-solve invariant. No lock directory, including package extra files, may be
written:

  $ dune pkg lock
  Error: Multi-platform solving selected different versions of the same package
  on different platforms. This is not supported.
  The following packages have version conflicts:
  - foo:version 1 on:- arch = arm64; os = linux
                     - arch = x86_64; os = linux
    version 2 on:- arch = arm64; os = macos
                 - arch = x86_64; os = macos
  [1]

No partial lock directory is written:
  $ test ! -e dune.lock
