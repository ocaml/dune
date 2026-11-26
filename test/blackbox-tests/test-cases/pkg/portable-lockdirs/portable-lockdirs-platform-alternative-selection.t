A dependency disjunction may select different package names on different
platforms. Preserve each platform's selection when generating a portable lock
directory.

  $ mkrepo
  $ add_mock_repo_if_needed

Define an implementation for each operating system:
  $ LINUX_FILE=mock-opam-repository/packages/linux-impl/linux-impl.0.0.1/files/platform.txt
  $ MACOS_FILE=mock-opam-repository/packages/macos-impl/macos-impl.0.0.1/files/platform.txt
  $ mkdir -p "$(dirname "$LINUX_FILE")" "$(dirname "$MACOS_FILE")"
  $ echo linux >"$LINUX_FILE"
  $ echo macos >"$MACOS_FILE"

  $ mkpkg linux-impl <<EOF
  > available: os = "linux"
  > extra-files: [
  >   ["platform.txt" "md5=$(md5sum "$LINUX_FILE" | cut -f1 -d' ')"]
  > ]
  > build: [
  >   ["mkdir" "-p" share "%{lib}%/%{name}%"]
  >   ["touch" "%{lib}%/%{name}%/META"]
  > ]
  > EOF

  $ mkpkg macos-impl <<EOF
  > available: os = "macos"
  > extra-files: [
  >   ["platform.txt" "md5=$(md5sum "$MACOS_FILE" | cut -f1 -d' ')"]
  > ]
  > build: [
  >   ["mkdir" "-p" share "%{lib}%/%{name}%"]
  >   ["touch" "%{lib}%/%{name}%/META"]
  > ]
  > EOF

The common package accepts either implementation:

  $ mkpkg foo <<'EOF'
  > depends: [ "linux-impl" | "macos-impl" ]
  > build: [
  >   ["mkdir" "-p" share "%{lib}%/%{name}%"]
  >   ["touch" "%{lib}%/%{name}%/META"]
  > ]
  > EOF

  $ make_portable_lockdirs_project
  $ dune pkg lock
  Solution for dune.lock
  
  Dependencies common to all supported platforms:
  - foo.0.0.1
  
  Additionally, some packages will only be built on specific platforms.
  
  arch = arm64; os = linux:
  - linux-impl.0.0.1
  
  arch = arm64; os = macos:
  - macos-impl.0.0.1
  
  arch = x86_64; os = linux:
  - linux-impl.0.0.1
  
  arch = x86_64; os = macos:
  - macos-impl.0.0.1

The lock directory contains the extra files of both selected alternatives:

  $ cat dune.lock/linux-impl.0.0.1.files/platform.txt
  linux
  $ cat dune.lock/macos-impl.0.0.1.files/platform.txt
  macos

The lock directory selects and builds only the appropriate implementation for
each platform:

  $ DUNE_CONFIG__OS=linux DUNE_CONFIG__ARCH=arm64 DUNE_CONFIG__OS_FAMILY=debian DUNE_CONFIG__OS_DISTRIBUTION=ubuntu DUNE_CONFIG__OS_VERSION=24.11 dune build
  $ ls $pkg_root/
  foo
  linux-impl

  $ dune clean

  $ DUNE_CONFIG__OS=macos DUNE_CONFIG__ARCH=x86_64 DUNE_CONFIG__OS_FAMILY=homebrew DUNE_CONFIG__OS_DISTRIBUTION=homebrew DUNE_CONFIG__OS_VERSION=15.3.1 dune build
  $ ls $pkg_root/
  foo
  macos-impl
