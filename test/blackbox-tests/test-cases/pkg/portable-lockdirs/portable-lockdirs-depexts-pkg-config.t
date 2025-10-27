Exercise generating depexts in portable lockdirs by solving a project that
depends on a simplified copy of conf-pkg-config - a package whose depexts vary
greatly depending on the platform.

  $ . ../helpers.sh
  $ mkrepo
  $ add_mock_repo_if_needed

Create a package based on the conf-pkg-config opam package. This package has a
depext on the native pkg-config, which is named differently depending on the os
distribution.
  $ mkpkg conf-pkg-config <<EOF
  > depexts: [
  >   ["pkg-config"] {os-family = "debian" | os-family = "ubuntu"}
  >   ["pkgconf"] {os = "macos" & os-distribution = "homebrew"}
  >   ["pkgconfig"] {os-distribution = "centos" & os-version <= "7"}
  >   ["pkgconf-pkg-config"] {os-distribution = "centos" & os-version >= "8"}
  > ]
  > EOF

Create a custom dune-workspace to constrain the number of platforms. Of note is
the fact that when solving for linux the os distribution is omitted. This test
will demonstrate that even though the project isn't solved for a particular
linux distribution, enough information is stored in the lockdir so that the
correct depext names can be chosen for the current distro at build time.
  $ cat > dune-workspace <<EOF
  > (lang dune 3.20)
  > (pkg enabled)
  > (repository
  >  (name mock)
  >  (url "file://$(pwd)/mock-opam-repository"))
  > (lock_dir
  >  (repositories mock)
  >  (solve_for_platforms
  >   ((arch x86_64)
  >    (os macos)
  >    (os-distribution homebrew)
  >    (os-family homebrew))
  >   ((arch x86_64)
  >    (os linux))
  >   ((arch x86_64)
  >    (os win32)
  >    (os-distribution cygwin)
  >    (os-family windows))))
  > EOF

  $ cat > dune-project <<EOF
  > (lang dune 3.18)
  > (package
  >  (name x)
  >  (depends conf-pkg-config))
  > EOF

  $ DUNE_CONFIG__PORTABLE_LOCK_DIR=enabled dune pkg lock
  Solution for dune.lock
  
  This solution supports the following platforms:
  - arch = x86_64; os = macos; os-distribution = homebrew; os-family = homebrew
  - arch = x86_64; os = linux
  - arch = x86_64; os = win32; os-distribution = cygwin; os-family = windows
  
  Dependencies on all supported platforms:
  - conf-pkg-config.0.0.1

Print the name of the depext on a variety of os/distro/versions:
  $ DUNE_CONFIG__OS=macos DUNE_CONFIG__OS_FAMILY=homebrew DUNE_CONFIG__OS_DISTRIBUTION=homebrew dune show depexts
  pkgconf
  $ DUNE_CONFIG__OS=linux DUNE_CONFIG__OS_FAMILY=debian dune show depexts
  pkg-config
  $ DUNE_CONFIG__OS=linux DUNE_CONFIG__OS_FAMILY=centos DUNE_CONFIG__OS_DISTRIBUTION=centos DUNE_CONFIG__OS_VERSION=6 dune show depexts
  pkgconfig
  $ DUNE_CONFIG__OS=linux DUNE_CONFIG__OS_FAMILY=centos DUNE_CONFIG__OS_DISTRIBUTION=centos DUNE_CONFIG__OS_VERSION=9 dune show depexts
  pkgconf-pkg-config
