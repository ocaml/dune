Build-time conditional package selection uses the selected lock stanza's solver
environment rather than the host platform.

  $ mkrepo
  $ add_mock_repo_if_needed

Create a package whose build result records the selected operating system.

  $ mkpkg foo <<'EOF'
  > build: [
  >   ["mkdir" "-p" share "%{lib}%/%{name}%"]
  >   ["touch" "%{lib}%/%{name}%/META"]
  >   ["sh" "-c" "echo Darwin > %{share}%/kernel"] { os = "macos" }
  >   ["sh" "-c" "echo Linux > %{share}%/kernel"] { os = "linux" }
  > ]
  > EOF

  $ make_portable_lockdirs_project
  $ cat >dune-workspace <<EOF
  > (lang dune 3.20)
  > (repository
  >  (name mock)
  >  (url "file://$(pwd)/mock-opam-repository"))
  > (lock_dir
  >  (repositories mock)
  >  (solver_env (os macos)))
  > (pkg enabled)
  > EOF

Although the host is Linux, the build context selects the macOS package fields.

  $ DUNE_CONFIG__PORTABLE_LOCK_DIR=enabled dune pkg lock >/dev/null 2>&1
  $ dune build
  $ cat $pkg_root/$(dune pkg print-digest foo)/target/share/kernel
  Darwin
