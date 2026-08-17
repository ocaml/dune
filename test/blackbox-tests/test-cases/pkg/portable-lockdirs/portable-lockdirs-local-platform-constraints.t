A local package's dependency filters are evaluated in each requested platform's
environment.

  $ mkrepo
  $ add_mock_repo_if_needed

  $ mkpkg foo 1
  $ mkpkg foo 2

The conflict excludes foo.2 everywhere except Linux. The Linux solve must retain
the platform filter instead of evaluating it in a platform-less environment.

  $ cat >dune-project <<'EOF'
  > (lang dune 3.18)
  > EOF
  $ cat >dune-workspace <<EOF
  > (lang dune 3.20)
  > (repository
  >  (name mock)
  >  (url "file://$(pwd)/mock-opam-repository"))
  > (lock_dir
  >  (repositories mock)
  >  (solve_for_platforms
  >   ((arch x86_64)
  >    (os linux))))
  > (pkg enabled)
  > EOF
  $ cat >x.opam <<'EOF'
  > opam-version: "2.0"
  > depends: [ "foo" {>= "2"} ]
  > conflicts: [ "foo" {>= "2" & os != "linux"} ]
  > EOF

  $ dune pkg lock
  Solution for dune.lock
  
  Dependencies common to all supported platforms:
  - foo.2
