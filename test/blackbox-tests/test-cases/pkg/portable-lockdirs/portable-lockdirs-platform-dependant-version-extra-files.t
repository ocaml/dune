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

The cross-platform version constraint rejects the disagreement on foo. No lock
directory, including package extra files, may be written:

  $ dune pkg lock
  Error:
  Unable to solve dependencies while generating lock directory: dune.lock
  
  The dependency solver failed to find a solution for the requested platforms:
  - arch = x86_64; os = linux
  - arch = arm64; os = linux
  - arch = x86_64; os = macos
  - arch = arm64; os = macos
  ...with this error:
  Couldn't solve the package dependency formula.
  Selected candidates: bar.0.0.1 x.dev
  - foo -> foo.1 on arch = arm64; os = linux
      bar 0.0.1 requires = 1
  - foo -> (problem) on arch = arm64; os = macos
      bar 0.0.1 requires = 2
      Rejected candidates:
        foo.2:
          Reason for rejection unknown:
          bar.0.0.1=true && foo.2=false => (no solution found)=true
        foo.1: Incompatible with restriction: = 2
  - foo -> foo.1 on arch = x86_64; os = linux
      bar 0.0.1 requires = 1
  - foo -> (problem) on arch = x86_64; os = macos
      bar 0.0.1 requires = 2
      Rejected candidates:
        foo.2:
          Reason for rejection unknown:
          bar.0.0.1=true && foo.2=false => (no solution found)=true
        foo.1: Incompatible with restriction: = 2
  Hint: If you don't need support for every requested platform, change
  Hint: (solve_for_platforms ...) in dune-workspace to only include the
  Hint: platforms you need, then rerun 'dune pkg lock'
  [1]

No partial lock directory is written:
  $ test ! -e dune.lock
