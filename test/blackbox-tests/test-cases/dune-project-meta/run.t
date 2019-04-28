Test the various new fields inside the dune-project file.

The `dune build` should work.

  $ dune build @install --root test-fields --auto-promote
  Entering directory 'test-fields'
  File "cohttp-async.opam", line 1, characters 0-0:
  Files _build/default/cohttp-async.opam and _build/default/cohttp-async.opam.expected differ.
  File "cohttp.opam", line 1, characters 0-0:
  Files _build/default/cohttp.opam and _build/default/cohttp.opam.expected differ.
  Promoting _build/default/cohttp-async.opam.expected to cohttp-async.opam.
  Promoting _build/default/cohttp.opam.expected to cohttp.opam.
  [1]
  $ cat test-fields/cohttp.opam
  opam-version: "2.0"
  dev-repo: "git+https://github.com/mirage/ocaml-cohttp.git"
  authors: ["Anil Madhavapeddy" "Rudi Grinberg"]
  license: "ISC"
  depends: [
    "alcotest" {with-test}
    "dune" {build & > "1.5"}
    "foo" {dev & > "1.5" & < "2.0"}
    "uri" {>= "1.9.0"}
    "uri" {< "2.0.0"}
    "fieldslib" {> "v0.12"}
    "fieldslib" {< "v0.13"}
    "ocaml" {>= "4.06.0"}
    "cohttp" {>= "1.0.0"}
  ]
  description: "A longer description"
  synopsis: "An OCaml library for HTTP clients and servers"
  $ cat test-fields/cohttp-async.opam
  opam-version: "2.0"
  dev-repo: "git+https://github.com/mirage/ocaml-cohttp.git"
  authors: ["Anil Madhavapeddy" "Rudi Grinberg"]
  license: "ISC"
  depends: [
    "cohttp" {>= "1.0.2"}
    "conduit-async" {>= "1.0.3"}
    "async" {>= "v0.10.0"}
    "async" {< "v0.12"}
    "ocaml" {>= "4.06.0"}
    "cohttp" {>= "1.0.0"}
  ]
  description: "A _really_ long description"
  synopsis: "HTTP client and server for the Async library"


Fatal error with invalid opam file:
  $ dune build @install --root bad-opam-file --auto-promote
  Entering directory 'bad-opam-file'
  File "foo.opam", line 1, characters 0-0:
  Warning: Unable to read opam file. This package's version field willbe ignored.
  Reason: File "foo.opam", line 1, characters 7-12:
  Parse error
  
  File "_build/default/foo.opam", line 1, characters 7-12:
  1 | cannot parse me
             ^^^^^
  Error: Parse error
  [1]
