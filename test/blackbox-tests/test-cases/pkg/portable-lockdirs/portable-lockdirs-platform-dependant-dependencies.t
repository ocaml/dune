Test for a project whose dependencies are different depending on the platform.

  $ . ../helpers.sh
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

  $ DUNE_CONFIG__PORTABLE_LOCK_DIR=enabled dune pkg lock
  Solution for dune.lock:
  - foo.0.0.1
  - linux-only.0.0.1
  - macos-only.0.0.1

Build the project as if we were on linux and confirm that only the linux-specific dependency is installed:
  $ DUNE_CONFIG__OS=linux DUNE_CONFIG__ARCH=arm64 DUNE_CONFIG__OS_FAMILY=debian DUNE_CONFIG__OS_DISTRIBUTION=ubuntu DUNE_CONFIG__OS_VERSION=24.11 dune build
  $ ls $pkg_root/
  foo
  linux-only

  $ dune clean

Build the project as if we were on macos and confirm that only the macos-specific dependency is installed:
  $ DUNE_CONFIG__OS=macos DUNE_CONFIG__ARCH=x86_64 DUNE_CONFIG__OS_FAMILY=homebrew DUNE_CONFIG__OS_DISTRIBUTION=homebrew DUNE_CONFIG__OS_VERSION=15.3.1 dune build
  $ ls $pkg_root/
  foo
  macos-only
