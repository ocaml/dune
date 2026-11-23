Explicit solve_for_platforms entries take precedence over conflicting platform
variables in the lock stanza's solver_env.

  $ mkrepo
  $ add_mock_repo_if_needed

  $ mkpkg linux-only <<'EOF'
  > available: os = "linux"
  > EOF
  $ mkpkg macos-only <<'EOF'
  > available: os = "macos"
  > EOF

  $ cat >dune-project <<'EOF'
  > (lang dune 3.18)
  > EOF
  $ cat >x.opam <<'EOF'
  > opam-version: "2.0"
  > depends: [
  >   "linux-only" {os = "linux"}
  >   "macos-only" {os = "macos"}
  > ]
  > EOF
  $ write_portable_lockdir_workspace <<'EOF'
  > (lock_dir
  >  (repositories mock)
  >  (solver_env (os windows))
  >  (solve_for_platforms
  >   ((arch x86_64) (os linux))
  >   ((arch x86_64) (os macos))))
  > EOF

  $ DUNE_CONFIG__PORTABLE_LOCK_DIR=enabled dune pkg lock
  Solution for dune.lock
  
  Dependencies common to all supported platforms:
  (none)
  
  Additionally, some packages will only be built on specific platforms.
  
  arch = x86_64; os = linux:
  - linux-only.0.0.1
  
  arch = x86_64; os = macos:
  - macos-only.0.0.1
