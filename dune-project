(lang dune 3.12)

;          ^^^^
; When changing the version, don't forget to regenerate *.opam files
; by running [dune build].

(name dune)

(generate_opam_files true)

(accept_alternative_dune_file_name)

(strict_package_deps false)

; Reserved for Dune itself. This is to help with the bootstrap

(using dune-bootstrap-info 0.1)

(license MIT)

(maintainers "Jane Street Group, LLC <opensource@janestreet.com>")

(authors "Jane Street Group, LLC <opensource@janestreet.com>")

(source
 (github ocaml/dune))

(documentation "https://dune.readthedocs.io/")

(implicit_transitive_deps false)

(package
 (name dune)
 (synopsis "Fast, portable, and opinionated build system")
 ; The "depends" and "build" field are written in dune.opam.template
 (conflicts
  (merlin
   (< 3.4.0))
  (ocaml-lsp-server
   (< 1.3.0))
  (dune-configurator
   (< 2.3.0))
  (odoc
   (< 2.0.1))
  (dune-release
   (< 1.3.0))
  (js_of_ocaml-compiler
   (< 3.6.0))
  (jbuilder
   (= transition)))
 (description
  "\nDune is a build system that was designed to simplify the release of\nJane Street packages. It reads metadata from \"dune\" files following a\nvery simple s-expression syntax.\n\nDune is fast, has very low-overhead, and supports parallel builds on\nall platforms. It has no system dependencies; all you need to build\ndune or packages using dune is OCaml. You don't need make or bash\nas long as the packages themselves don't use bash explicitly.\n\nDune is composable; supporting multi-package development by simply\ndropping multiple repositories into the same directory.\n\nDune also supports multi-context builds, such as building against\nseveral opam roots/switches simultaneously. This helps maintaining\npackages across several versions of OCaml and gives cross-compilation\nfor free.\n"))

(package
 (name dune-build-info)
 (synopsis "Embed build information inside executable")
 (depends
  (ocaml
   (>= 4.08)))
 (description
  "The build-info library allows to access information about how the\nexecutable was built, such as the version of the project at which it\nwas built or the list of statically linked libraries with their\nversions.  It supports reporting the version from the version control\nsystem during development to get an precise reference of when the\nexecutable was built.\n"))

(package
 (name dune-private-libs)
 (synopsis "Private libraries of Dune")
 (depends
  (csexp
   (>= 1.5.0))
  (pp
   (>= 1.1.0))
  (dyn
   (= :version))
  (stdune
   (= :version))
  (ocaml
   (>= 4.08)))
 (description
  "!!!!!!!!!!!!!!!!!!!!!!\n!!!!! DO NOT USE !!!!!\n!!!!!!!!!!!!!!!!!!!!!!\n\nThis package contains code that is shared between various dune-xxx\npackages. However, it is not meant for public consumption and provides\nno stability guarantee.\n"))

(package
 (name dune-configurator)
 (synopsis "Helper library for gathering system configuration")
 (depends
  (ocaml
   (>= 4.04.0))
  base-unix
  (csexp
   (>= 1.5.0)))
 (description
  "dune-configurator is a small library that helps writing OCaml scripts that\ntest features available on the system, in order to generate config.h\nfiles for instance.\nAmong other things, dune-configurator allows one to:\n- test if a C program compiles\n- query pkg-config\n- import #define from OCaml header files\n- generate config.h file\n"))

(package
 (name dune-action-plugin)
 (synopsis "[experimental] API for writing dynamic Dune actions")
 (depends
  (dune-glob
   (= :version))
  (csexp
   (>= 1.5.0))
  (ppx_expect :with-test)
  (stdune
   (= :version))
  (dune-private-libs
   (= :version))
  (dune-rpc
   (= :version))
  base-unix)
 (description
  "\nThis library is experimental. No backwards compatibility is implied.\n\ndune-action-plugin provides an API for writing dynamic Dune actions.\nDynamic dune actions do not need to declare their dependencies\nupfront; they are instead discovered automatically during the\nexecution of the action.\n"))

(package
 (name dune-glob)
 (synopsis "Glob string matching language supported by dune")
 (depends
  (stdune
   (= :version))
  dyn
  ordering
  (dune-private-libs
   (= :version)))
 (description
  "dune-glob provides a parser and interpreter for globs as understood by dune language."))

(package
 (name dune-site)
 (synopsis "Embed locations information inside executable and libraries")
 (depends
  (dune-private-libs
   (= :version)))
 (description ""))

(package
 (name dune-rpc)
 (synopsis "Communicate with dune using rpc")
 (depends
  csexp
  ordering
  dyn
  xdg
  (stdune
   (= :version))
  (pp
   (>= 1.1.0)))
 (description "Library to connect and control a running dune instance"))

(package
 (name dune-rpc-lwt)
 (synopsis "Communicate with dune using rpc and Lwt")
 (depends
  (dune-rpc
   (= :version))
  (csexp
   (>= 1.5.0))
  (lwt
   (>= 5.6.0))
  base-unix)
 (description "Specialization of dune-rpc to Lwt"))

(package
 (name dyn)
 (synopsis "Dynamic type")
 (depends
  (ocaml
   (>= 4.08.0))
  (ordering
   (= :version))
  (pp
   (>= 1.1.0)))
 (description "Dynamic type"))

(package
 (name ordering)
 (synopsis "Element ordering")
 (depends
  (ocaml
   (>= 4.08.0)))
 (description "Element ordering"))

(package
 (name xdg)
 (synopsis "XDG Base Directory Specification")
 (depends
  (ocaml
   (>= 4.08)))
 (description
  "https://specifications.freedesktop.org/basedir-spec/basedir-spec-latest.html"))

(package
 (name stdune)
 (synopsis "Dune's unstable standard library")
 (depends
  (ocaml
   (>= 4.08.0))
  base-unix
  (dyn
   (= :version))
  (ordering
   (= :version))
  (pp
   (>= 1.2.0))
  (csexp
   (>= 1.5.0)))
 (description
  "This library offers no backwards compatibility guarantees. Use at your own risk."))

(package
 (name ocamlc-loc)
 (synopsis "Parse ocaml compiler output into structured form")
 (conflicts
  (ocaml-lsp-server
   (< 1.15.0)))
 (depends
  (ocaml
   (>= 4.08.0))
  (dyn
   (= :version)))
 (description
  "This library offers no backwards compatibility guarantees. Use at your own risk."))

(package
 (name chrome-trace)
 (synopsis "Chrome trace event generation library")
 (depends
  (ocaml
   (>= 4.08.0)))
 (description
  "This library offers no backwards compatibility guarantees. Use at your own risk."))
