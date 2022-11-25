The project "root_project" vendor "a" and "b" project.
But "a" is already vendored by "b" (that result as same package).
  $ dune build
  Error: Too many opam files for package "a":
  - a/a.opam
  - b/a/a.opam
  [1]
