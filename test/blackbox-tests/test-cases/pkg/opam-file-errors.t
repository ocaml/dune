Tests for error messages while reading package metadata from opam files

  $ mkrepo
  $ add_mock_repo_if_needed

  $ cat >dune-project <<EOF
  > (lang dune 3.12)
  > EOF

Empty files are not allowed as they lack the format version.
  $ touch x.opam
  $ dune pkg lock
  File "x.opam", line 1, characters 0-0:
  Error: unexpected version
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
  File "x.opam", line 1, characters 0-0:
  Warning: Unable to read opam file. Some information about this package such
  as its version will be ignored.
  Reason: File "x.opam", line 3, characters 10-11:
  Error: Parse error
  
  File "$TESTCASE_ROOT/x.opam", line 3, characters 10-11:
  3 |   "a" { < ("0.1" & "0.2") }
                ^
  Error: unable to parse opam file
  Parse error
  [1]
