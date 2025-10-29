Demonstrate various cases representing depexts in lockfiles.

  $ . ../helpers.sh
  $ mkrepo
  $ add_mock_repo_if_needed

  $ mkpkg foo <<EOF
  > depexts: [
  >   # unconditional depexts (these are rare but opam allows them)
  >   ["foo" "bar" "baz"]
  >   # depexts depending on the distro
  >   ["foo-ubuntu" "bar-ubuntu"] {os-distribution = "ubuntu"}
  >   ["foo-arch" "bar-arch"] {os-distribution = "archlinux"}
  > ]
  > EOF

  $ cat > dune-project <<EOF
  > (lang dune 3.18)
  > (package
  >  (name x)
  >  (depends foo))
  > EOF

  $ DUNE_CONFIG__PORTABLE_LOCK_DIR=enabled dune pkg lock
  Solution for dune.lock
  
  This solution supports the following platforms:
  - arch = x86_64; os = linux
  - arch = arm64; os = linux
  - arch = x86_64; os = macos
  - arch = arm64; os = macos
  - arch = x86_64; os = win32
  
  Dependencies on all supported platforms:
  - foo.0.0.1

  $ cat ${default_lock_dir}/foo.0.0.1.pkg
  (version 0.0.1)
  
  (depexts
   (foo baz bar)
   ((foo-ubuntu bar-ubuntu)
    (= %{os_distribution} ubuntu))
   ((foo-arch bar-arch)
    (= %{os_distribution} archlinux)))
