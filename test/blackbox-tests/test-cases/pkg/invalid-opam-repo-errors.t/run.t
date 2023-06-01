Test the error cases for invalid opam repositories

  $ cat >dune-project <<EOF
  > (lang dune 3.8)
  > (package
  >  (name lockfile_generation_test))
  > EOF

  $ dune pkg lock --opam-env=pure --opam-repository=directory-that-does-not-exist
  Error: directory-that-does-not-exist does not exist
  [1]

  $ touch empty
  $ dune pkg lock --opam-env=pure --opam-repository=empty
  Error: empty is not a directory
  [1]

  $ dune pkg lock --opam-env=pure --opam-repository=no-packages-dir
  Error: no-packages-dir doesn't look like a path to an opam repository as it
  lacks a subdirectory named "packages"
  [1]

  $ dune pkg lock --opam-env=pure --opam-repository=no-repo-file
  Error: File no-repo-file/repo does not exist or can't be read
  [1]

  $ dune pkg lock --opam-env=pure --opam-repository=bad-repo-file
  Error: At bad-repo-file/repo:1:4-1:8::
  Parse error
  [1]

  $ dune pkg lock --opam-env=pure --opam-repository=no-repo-version
  Error: The file no-repo-version/repo lacks an "opam-version" field.
  Hint: Add `opam-version: "2.0"` to the file.
  [1]

  $ dune pkg lock --opam-env=pure --opam-repository=bad-repo-version
  Error: The file bad-repo-version/repo specifies an opam-version which is too
  low (1.0). The minimum opam-version is 2.0.
  Hint: Change the opam-version field to `opam-version: "2.0"`.
  [1]
