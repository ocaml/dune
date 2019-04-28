Test the various new fields inside the dune-project file.

The `dune build` should work.

  $ dune build @install --root test-fields --auto-promote
  Entering directory 'test-fields'
  $ cat test-fields/cohttp.opam
  opam-version: "2.0"
  build: [
    ["dune" "subst"] {pinned}
    ["dune" "build" "-p" name "-j" jobs]
    ["dune" "runtest" "-p" name "-j" jobs] {with-test}
    ["dune" "build" "-p" name "@doc"] {with-doc}
  ]
  authors: ["Anil Madhavapeddy" "Rudi Grinberg"]
  bug-reports: "https://github.com/mirage/ocaml-cohttp/issues"
  homepage: "https://github.com/mirage/ocaml-cohttp"
  license: "ISC"
  dev-repo: "git+https://github.com/mirage/ocaml-cohttp.git"
  synopsis: "An OCaml library for HTTP clients and servers"
  description: "A longer description"
  depends: [
    "alcotest" {with-test}
    "dune" {build & > "1.5"}
    "foo" {dev & > "1.5" & < "2.0"}
    "uri" {>= "1.9.0"}
    "uri" {< "2.0.0"}
    "fieldslib" {> "v0.12"}
    "fieldslib" {< "v0.13"}
  ]
  $ cat test-fields/cohttp-async.opam
  opam-version: "2.0"
  build: [
    ["dune" "subst"] {pinned}
    ["dune" "build" "-p" name "-j" jobs]
    ["dune" "runtest" "-p" name "-j" jobs] {with-test}
    ["dune" "build" "-p" name "@doc"] {with-doc}
  ]
  authors: ["Anil Madhavapeddy" "Rudi Grinberg"]
  bug-reports: "https://github.com/mirage/ocaml-cohttp/issues"
  homepage: "https://github.com/mirage/ocaml-cohttp"
  license: "ISC"
  dev-repo: "git+https://github.com/mirage/ocaml-cohttp.git"
  synopsis: "HTTP client and server for the Async library"
  description: "A _really_ long description"
  depends: [
    "cohttp" {>= "1.0.2"}
    "conduit-async" {>= "1.0.3"}
    "async" {>= "v0.10.0"}
    "async" {< "v0.12"}
  ]


Fatal error with opam file that is not listed in the dune-project file:

  $ dune build @install --root bad-opam-file --auto-promote
  Entering directory 'bad-opam-file'
  File "foo.opam", line 1, characters 0-0:
  Error: This opam file doesn't have a corresponding (package ...) stanza in the
  dune-project_file. Since you have at least one other (package ...) stanza in
  your dune-project file, you must a (package ...) stanza for each opam package
  in your project.
  [1]
