Exercise dune resolving the post dependencies found in compiler packages.

  $ mkrepo

  $ cat >dune-workspace << EOF
  > (lang dune 3.20)
  > (pkg enabled)
  > (lock_dir
  >  (path dune.lock)
  >  (repositories mock)
  >  (solver_env
  >   (os linux)))
  > EOF

  $ mkpkg host-system-other

  $ mkpkg system-mingw << EOF
  > available: os = "win32"
  > EOF

  $ mkpkg ocaml << EOF
  > depends: [
  >   "ocaml-base-compiler"
  > ]
  > EOF

This package has a dependency cycle with the "ocaml" package, broken
by the use of post dependencies. It also contains a formula with no
solutions when the "post" variable is set to "false" on non-windows
systems. Opam evaluates dependency formulae with post=true and then
does further filtering to remove post dependencies from dependency
lists to prevent circular dependencies at package build time.
  $ mkpkg ocaml-base-compiler << EOF
  > depends: [
  >  "ocaml" {post}
  >   (("arch-x86_64" {os = "win32" & arch = "x86_64"} & "system-mingw" &
  >     "mingw-w64-shims" {os-distribution = "cygwin" & build}) |
  >    ("arch-x86_32" {os = "win32"} & "ocaml-option-bytecode-only" &
  >     "system-mingw" &
  >     "mingw-w64-shims" {os-distribution = "cygwin" & build}) |
  >    "host-system-other" {os != "win32" & post})
  > ]
  > EOF

  $ solve ocaml-base-compiler 
  Solution for dune.lock:
  - ocaml-base-compiler.0.0.1

Ensure that packages can be resolved at build time. This checks that
the dependency cycle between the "ocaml" and "ocaml-base-compiler"
packages is successfully broken by the use of post dependencies.
  $ dune build
