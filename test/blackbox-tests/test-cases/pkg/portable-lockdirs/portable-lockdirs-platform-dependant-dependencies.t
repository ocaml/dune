Test for a project whose dependencies are different depending on the platform.

  $ mkrepo
  $ add_mock_repo_if_needed

A package that's only available on linux:
  $ mkpkg linux-only <<EOF
  > available: os = "linux"
  > build: [
  >   ["mkdir" "-p" share "%{lib}%/%{name}%"]
  >   ["touch" "%{lib}%/%{name}%/META"] # needed for dune to recognize this as a library
  > ]
  > EOF

A package that's only available on macos:
  $ mkpkg macos-only <<EOF
  > available: os = "macos"
  > build: [
  >   ["mkdir" "-p" share "%{lib}%/%{name}%"]
  >   ["touch" "%{lib}%/%{name}%/META"] # needed for dune to recognize this as a library
  > ]
  > EOF

A package that conditionally depends on packages depending on the OS:
  $ mkpkg foo <<EOF
  > build: [
  >   ["mkdir" "-p" share "%{lib}%/%{name}%"]
  >   ["touch" "%{lib}%/%{name}%/META"] # needed for dune to recognize this as a library
  > ]
  > depends: [
  >  "linux-only" { os = "linux" }
  >  "macos-only" { os = "macos" }
  > ]
  > EOF

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

  $ dune pkg lock
  Solution for dune.lock
  
  Dependencies common to all supported platforms:
  - foo.0.0.1
  
  Additionally, some packages will only be built on specific platforms.
  
  arch = arm64; os = linux:
  - linux-only.0.0.1
  
  arch = arm64; os = macos:
  - macos-only.0.0.1
  
  arch = x86_64; os = linux:
  - linux-only.0.0.1
  
  arch = x86_64; os = macos:
  - macos-only.0.0.1

Build the project as if we were on linux and confirm that only the linux-specific dependency is installed:
  $ DUNE_CONFIG__OS=linux DUNE_CONFIG__ARCH=arm64 DUNE_CONFIG__OS_FAMILY=debian DUNE_CONFIG__OS_DISTRIBUTION=ubuntu DUNE_CONFIG__OS_VERSION=24.11 dune build
  $ ls $pkg_root/
  foo.0.0.1-dd2053df7e4195c21d4ca02e3382a72c
  linux-only.0.0.1-1f9f3882dc7fc80b255d673f9388a700

  $ dune clean

Build the project as if we were on macos and confirm that only the macos-specific dependency is installed:
  $ DUNE_CONFIG__OS=macos DUNE_CONFIG__ARCH=x86_64 DUNE_CONFIG__OS_FAMILY=homebrew DUNE_CONFIG__OS_DISTRIBUTION=homebrew DUNE_CONFIG__OS_VERSION=15.3.1 dune build
  $ ls $pkg_root/
  foo.0.0.1-9239511d918ee88a97a5111113de4120
  macos-only.0.0.1-916cbf8066ac1320a64e5235170b490e
