(lang dune 1.10)
(name dune)

(implicit_transitive_deps false)
(generate_opam_files true)

(license MIT)
(maintainers "Jane Street Group, LLC <opensource@janestreet.com>")
(authors "Jane Street Group, LLC <opensource@janestreet.com>")
(source (github ocaml/dune))

(package
 (name dune)
 (depends
  (ocaml (>= 4.02))
  base-unix
  base-threads)
 (conflicts
  (jbuilder (<> "transition"))
  (odoc (< 1.3.0)))
 (synopsis "Fast, portable and opinionated build system")
 (description "
dune is a build system that was designed to simplify the release of
Jane Street packages. It reads metadata from \"dune\" files following a
very simple s-expression syntax.

dune is fast, it has very low-overhead and support parallel builds on
all platforms. It has no system dependencies, all you need to build
dune and packages using dune is OCaml. You don't need or make or bash
as long as the packages themselves don't use bash explicitly.

dune supports multi-package development by simply dropping multiple
repositories into the same directory.

It also supports multi-context builds, such as building against
several opam roots/switches simultaneously. This helps maintaining
packages across several versions of OCaml and gives cross-compilation
for free.
"))
