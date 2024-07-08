Reproduces https://github.com/ocaml/dune/issues/10670

Dune solves packages with "post" set to false, so the disjunction in
ocaml-base-compiler's dependencies can't be resolved on non-windows
systems.

  $ . ./helpers.sh
  $ mkrepo

  $ cat >dune-workspace << EOF
  > (lang dune 3.16)
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

  $ mkpkg ocaml-base-compiler << EOF
  > depends: [
  >   (("arch-x86_64" {os = "win32" & arch = "x86_64"} & "system-mingw" &
  >     "mingw-w64-shims" {os-distribution = "cygwin" & build}) |
  >    ("arch-x86_32" {os = "win32"} & "ocaml-option-bytecode-only" &
  >     "system-mingw" &
  >     "mingw-w64-shims" {os-distribution = "cygwin" & build}) |
  >    "host-system-other" {os != "win32" & post})
  > ]
  > EOF

  $ solve ocaml-base-compiler 
  Error: Unable to solve dependencies for the following lock directories:
  Lock directory dune.lock:
  Can't find all required versions.
  Selected: ocaml-base-compiler.0.0.1 x.dev system-mingw
  - system-mingw -> (problem)
      No usable implementations:
        system-mingw.0.0.1: Availability condition not satisfied
  [1]
