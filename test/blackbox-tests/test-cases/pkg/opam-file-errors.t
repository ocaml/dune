Tests for error messages while reading package metadata from opam files
  $ . ./helpers.sh
  $ mkrepo
  $ add_mock_repo_if_needed

  $ cat >dune-project <<EOF
  > (lang dune 3.12)
  > EOF

Empty files are not allowed as they lack the format version.
  $ touch x.opam
  $ dune pkg lock
  Error: Unable to parse opam file x.opam as local dune package.
  unsupported or missing file format version; should be 2.0 or older
  [1]

Handle the case where opam's parser rejects the file.
  $ cat > x.opam <<EOF
  > opam-version: "2.0"
  > depends: [
  >   "a" { < ("0.1" & "0.2") }
  > ]
  > EOF
  $ dune pkg lock
  File "$TESTCASE_ROOT/x.opam", line 3, characters 10-11:
  3 |   "a" { < ("0.1" & "0.2") }
                ^
  Error: Parse error
  [1]

Make sure we print an error when encountering opam files with dependency
specifications that can't be represented by dune's package metadata format.
  $ cat > x.opam <<EOF
  > opam-version: "2.0"
  > depends: [
  >   "a" | "b"
  > ]
  > EOF
  $ dune pkg lock
  Solution for dune.lock:
  (no dependencies to lock)

  $ cat > x.opam <<EOF
  > opam-version: "2.0"
  > depends: [
  >   "a" { "foo" }
  > ]
  > EOF
  $ dune pkg lock
  Solution for dune.lock:
  (no dependencies to lock)

  $ cat > x.opam <<EOF
  > opam-version: "2.0"
  > depends: [
  >   "a" { < foo:bar }
  > ]
  > EOF
  $ dune pkg lock
  Solution for dune.lock:
  (no dependencies to lock)
