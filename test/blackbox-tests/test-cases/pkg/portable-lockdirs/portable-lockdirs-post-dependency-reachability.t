A package retained in a portable lock directory must not keep a dependency that
was pruned as unreachable on its platform.

  $ mkrepo
  $ add_mock_repo_if_needed

On Linux, p is reachable only through a post dependency, so its dependency q
should be pruned. On macOS, p is a regular dependency but q is inactive.

  $ mkpkg p 1 <<'EOF'
  > depends: [ "q" {os = "linux"} ]
  > EOF
  $ mkpkg q 1 <<'EOF'
  > EOF

  $ cat >xlinux.opam <<'EOF'
  > opam-version: "2.0"
  > depends: [ "p" {os = "linux" & post} ]
  > EOF
  $ cat >xmac.opam <<'EOF'
  > opam-version: "2.0"
  > depends: [ "p" {os = "macos"} ]
  > EOF

  $ cat >dune-project <<'EOF'
  > (lang dune 3.20)
  > EOF
  $ cat >dune-workspace <<EOF
  > (lang dune 3.20)
  > (repository
  >  (name mock)
  >  (url "file://$(pwd)/mock-opam-repository"))
  > (lock_dir
  >  (repositories mock)
  >  (solve_for_platforms
  >   ((arch x86_64) (os linux))
  >   ((arch x86_64) (os macos))))
  > EOF

Locking succeeds, p is retained, and q is not retained.

  $ dune pkg lock >/dev/null 2>&1
  $ test -e dune.lock/p.1.pkg
  $ test ! -e dune.lock/q.1.pkg
