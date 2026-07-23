Test for a project which depends on different versions of the same package depending on the platform.

  $ mkrepo
  $ add_mock_repo_if_needed

Define 2 versions of the package foo that write their version number to a file
during their build so we can validate which version was built.

  $ mkpkg foo 1 <<EOF
  > build: [
  >   ["mkdir" "-p" share "%{lib}%/%{name}%"]
  >   ["touch" "%{lib}%/%{name}%/META"] # needed for dune to recognize this as a library
  >   ["sh" "-c" "echo %{version}% > %{share}%/version"]
  > ]
  > EOF
  $ mkpkg foo 2 <<EOF
  > build: [
  >   ["mkdir" "-p" share "%{lib}%/%{name}%"]
  >   ["touch" "%{lib}%/%{name}%/META"] # needed for dune to recognize this as a library
  >   ["sh" "-c" "echo %{version}% > %{share}%/version"]
  > ]
  > EOF

Define a package bar which conditionally depends on different versions of foo:

  $ make_platform_dependent_bar_package

  $ make_x_depends_bar_project

  $ dune pkg lock
  Solution for dune.lock
  
  Dependencies common to all supported platforms:
  - bar.0.0.1
  
  Additionally, some packages will only be built on specific platforms.
  
  arch = arm64; os = linux:
  - foo.1
  
  arch = arm64; os = macos:
  - foo.2
  
  arch = x86_64; os = linux:
  - foo.1
  
  arch = x86_64; os = macos:
  - foo.2

Build the project as if we were on linux and confirm that version 1 of foo was built:
  $ export DUNE_CONFIG__OS=linux DUNE_CONFIG__ARCH=arm64 DUNE_CONFIG__OS_FAMILY=debian DUNE_CONFIG__OS_DISTRIBUTION=ubuntu DUNE_CONFIG__OS_VERSION=24.11
  $ dune build
  $ cat $pkg_root/$(dune pkg print-digest foo)/target/share/version
  1

  $ dune clean

Build the project as if we were on macos and confirm that version 2 of foo was built:
  $ export DUNE_CONFIG__OS=macos DUNE_CONFIG__ARCH=x86_64 DUNE_CONFIG__OS_FAMILY=homebrew DUNE_CONFIG__OS_DISTRIBUTION=homebrew DUNE_CONFIG__OS_VERSION=15.3.1
  $ dune build
  $ cat $pkg_root/$(dune pkg print-digest foo)/target/share/version
  2

