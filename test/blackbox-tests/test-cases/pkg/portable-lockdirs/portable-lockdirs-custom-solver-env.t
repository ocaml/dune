Exercise solving with portable lockdirs when there is a custom solver
environment that affects the solution.

  $ . ../helpers.sh
  $ mkrepo
  $ add_mock_repo_if_needed

Create a workspace that defines a lockdir with a custom solver environment,
setting the variable "sys-ocaml-version":
  $ cat > dune-workspace <<EOF
  > (lang dune 3.20)
  > (pkg enabled)
  > (repository
  >  (name mock)
  >  (url "file://$(pwd)/mock-opam-repository"))
  > (lock_dir
  >  (path .dune-solution-cache)
  >  (repositories mock)
  >  (solver_env
  >   (sys-ocaml-version 5.4.0+solver-env-version-override)))
  > EOF

Create a package that creates a file only if "sys-ocaml-version" has a particular value:
  $ mkpkg foo <<EOF
  > build: [
  >   ["mkdir" "-p" share "%{lib}%/%{name}%"]
  >   ["touch" "%{lib}%/%{name}%/META"] # needed for dune to recognize this as a library
  >   ["sh" "-c" "echo %{sys-ocaml-version}% > %{share}%/sys-ocaml-version"] { sys-ocaml-version = "5.4.0+solver-env-version-override" }
  > ]
  > EOF

Set up a project that depends on the package:
  $ cat > dune-project <<EOF
  > (lang dune 3.18)
  > (package
  >  (name x)
  >  (depends foo))
  > EOF

  $ cat > x.ml <<EOF
  > let () = print_endline "Hello, World!"
  > EOF

  $ cat > dune <<EOF
  > (executable
  >  (public_name x)
  >  (libraries foo))
  > EOF

Solve the project:
  $ DUNE_CONFIG__PORTABLE_LOCK_DIR=enabled dune pkg lock
  Solution for .dune-solution-cache
  
  This solution supports the following platforms:
  - arch = x86_64; os = linux; sys-ocaml-version =
    5.4.0+solver-env-version-override
  - arch = arm64; os = linux; sys-ocaml-version =
    5.4.0+solver-env-version-override
  - arch = x86_64; os = macos; sys-ocaml-version =
    5.4.0+solver-env-version-override
  - arch = arm64; os = macos; sys-ocaml-version =
    5.4.0+solver-env-version-override
  - arch = x86_64; os = win32; sys-ocaml-version =
    5.4.0+solver-env-version-override
  
  Dependencies on all supported platforms:
  - foo.0.0.1

Confirming that the build action creates the conditional file:
  $ cat ${default_lock_dir}/foo.0.0.1.pkg
  (version 0.0.1)
  
  (build
   (all_platforms
    ((action
      (progn
       (run mkdir -p %{share} %{lib}/%{pkg-self:name})
       (run touch %{lib}/%{pkg-self:name}/META)
       (run sh -c "echo %{sys_ocaml_version} > %{share}/sys-ocaml-version"))))))

Build and print the file that was conditionally added. Note that the value of
"sys-ocaml-version" at solve-time may be different from "sys-ocaml-version" at
build-time, since at solve-time variables are taken from the portable lockdir
platform config and custom solver env, while at build-time variables are taken
from the current system. For platform variables like sys-ocaml-version, an
environment variable can be used to override the value that would otherwise be
read from the current system.
  $ export DUNE_CONFIG__SYS_OCAML_VERSION=5.4.0+solver-env-version-override
  $ dune build
  $ cat _build/_private/default/.pkg/$(dune pkg print-digest foo)/target/share/sys-ocaml-version
  5.4.0+solver-env-version-override
