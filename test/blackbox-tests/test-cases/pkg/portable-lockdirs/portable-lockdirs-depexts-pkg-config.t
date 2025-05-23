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
  > depends: [
  >   ("host-arch-x86_64" {os = "win32" & os-distribution = "msys2"} &
  >    "conf-mingw-w64-pkgconf-x86_64" {os = "win32" & os-distribution = "msys2"} |
  >    "host-arch-x86_32" {os = "win32" & os-distribution = "msys2"} &
  >    "conf-mingw-w64-pkgconf-i686" {os = "win32" & os-distribution = "msys2"})
  > ]
  > flags: conf
  > build: [
  >   ["pkg-config" "--help"]
  >     {os != "openbsd" & os != "win32" &
  >      !(os = "macos" & os-distribution = "homebrew")}
  >   ["pkgconf" "--version"]
  >     {os = "win32" & os-distribution != "msys2" |
  >      os = "macos" & os-distribution = "homebrew"}
  > ]
  > depexts: [
  >   ["pkg-config"] {os-family = "debian" | os-family = "ubuntu"}
  >   ["pkgconf"] {os-distribution = "arch"}
  >   ["pkgconf-pkg-config"] {os-distribution = "fedora"}
  >   ["pkgconfig"] {os-distribution = "centos" & os-version <= "7"}
  >   ["pkgconf-pkg-config"] {os-distribution = "mageia"}
  >   ["pkgconfig"] {os-distribution = "rhel" & os-version <= "7"}
  >   ["pkgconfig"] {os-distribution = "ol" & os-version <= "7"}
  >   ["pkgconf"] {os-distribution = "alpine"}
  >   ["pkg-config"] {os-distribution = "nixos"}
  >   ["pkgconf"] {os = "macos" & os-distribution = "homebrew"}
  >   ["pkgconfig"] {os = "macos" & os-distribution = "macports"}
  >   ["pkgconf"] {os = "freebsd"}
  >   ["pkgconf-pkg-config"] {os-distribution = "rhel" & os-version >= "8"}
  >   ["pkgconf-pkg-config"] {os-distribution = "centos" & os-version >= "8"}
  >   ["pkgconf-pkg-config"] {os-distribution = "ol" & os-version >= "8"}
  >   ["system:pkgconf"] {os = "win32" & os-distribution = "cygwinports"}
  >   ["pkgconf"] {os-distribution = "cygwin"}
  > ]
  > EOF

Create a custom dune-workspace to constrain the number of platforms. Of note is
the fact that when solving for linux the os distribution is omitted. This test
will demonstrate that even though the project isn't solved for a particular
linux distribution, enough information is stored in the lockdir so that the
correct depext names can be chosen for the current distro at build time.
  $ cat > dune-workspace <<EOF
  > (lang dune 3.18)
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
  Solution for dune.lock:
  - conf-pkg-config.0.0.1

Print the name of the depext on a variety of os/distro/versions:
  $ DUNE_CONFIG__ARCH=x86_64 DUNE_CONFIG__OS=macos DUNE_CONFIG__OS_DISTRIBUTION=homebrew dune show depexts
  pkgconf
  $ DUNE_CONFIG__ARCH=x86_64 DUNE_CONFIG__OS=linux DUNE_CONFIG__OS_FAMILY=debian dune show depexts
  pkg-config
  $ DUNE_CONFIG__ARCH=x86_64 DUNE_CONFIG__OS=linux DUNE_CONFIG__OS_FAMILY=centos DUNE_CONFIG__OS_DISTRIBUTION=centos DUNE_CONFIG__OS_VERSION=6 dune show depexts
  pkgconfig
  $ DUNE_CONFIG__ARCH=x86_64 DUNE_CONFIG__OS=linux DUNE_CONFIG__OS_FAMILY=centos DUNE_CONFIG__OS_DISTRIBUTION=centos DUNE_CONFIG__OS_VERSION=9 dune show depexts
  pkgconf-pkg-config
