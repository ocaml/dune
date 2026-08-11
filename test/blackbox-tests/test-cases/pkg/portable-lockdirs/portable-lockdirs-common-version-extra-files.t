Locking for multiple platforms preserves the extra files of the selected common
package version and omits files belonging only to rejected versions.

  $ mkrepo
  $ add_mock_repo_if_needed

  $ VERSION1_FILE=mock-opam-repository/packages/foo/foo.1/files/version.txt
  $ VERSION2_FILE=mock-opam-repository/packages/foo/foo.2/files/version.txt
  $ mkdir -p $(dirname $VERSION1_FILE) $(dirname $VERSION2_FILE)
  $ echo version_1 >$VERSION1_FILE
  $ echo version_2 >$VERSION2_FILE

  $ mkpkg foo 1 <<EOF
  > extra-files: [
  >   ["version.txt" "md5=$(md5sum $VERSION1_FILE | cut -f1 -d' ')" ]
  > ]
  > EOF
  $ mkpkg foo 2 <<EOF
  > extra-files: [
  >   ["version.txt" "md5=$(md5sum $VERSION2_FILE | cut -f1 -d' ')" ]
  > ]
  > EOF

  $ cat >dune-project <<'EOF'
  > (lang dune 3.18)
  > (package
  >  (name x)
  >  (depends (foo (= 1))))
  > EOF

  $ dune pkg lock
  Solution for dune.lock
  
  Dependencies common to all supported platforms:
  - foo.1

  $ cat ${default_lock_dir}/foo.1.files/version.txt
  version_1
  $ test ! -e ${default_lock_dir}/foo.2.files
