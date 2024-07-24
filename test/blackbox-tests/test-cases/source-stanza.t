Test 'source' staza compatibility with both user and organization paths from
the supported 'github', 'gitlab', 'sourcehut', and 'bitbucket'.

Test a generated 'github' user repo

  $ cat > dune-project <<EOF
  > (lang dune 3.15)
  > (name foo)
  > (generate_opam_files true)
  > (source (github user/repo))
  > (package
  >  (allow_empty)
  >  (name foo))
  > EOF

  $ dune build
  $ cat foo.opam | grep -i github
  homepage: "https://github.com/user/repo"
  bug-reports: "https://github.com/user/repo/issues"
  dev-repo: "git+https://github.com/user/repo.git"

Test a generated 'gitlab' user repo

  $ sed -i '4c\(source (gitlab user/repo))' dune-project
  $ dune build
  $ cat foo.opam | grep -i gitlab
  homepage: "https://gitlab.com/user/repo"
  bug-reports: "https://gitlab.com/user/repo/-/issues"
  dev-repo: "git+https://gitlab.com/user/repo.git"

Test a generated 'sourcehut' user repo

  $ sed -i '4c\(source (sourcehut user/repo))' dune-project
  $ dune build
  $ cat foo.opam | grep -i sr.ht
  homepage: "https://sr.ht/~user/repo"
  bug-reports: "https://todo.sr.ht/~user/repo"
  dev-repo: "git+https://git.sr.ht/~user/repo"

Test a generated 'bitbucket' ruser repo

  $ sed -i '4c\(source (bitbucket user/repo))' dune-project
  $ dune build
  $ cat foo.opam | grep -i bitbucket
  homepage: "https://bitbucket.org/user/repo"
  bug-reports: "https://bitbucket.org/user/repo/issues"
  dev-repo: "git+https://bitbucket.org/user/repo.git"

Test a generated 'gitlab' organization repo

  $ sed -i '4c\(source (gitlab organization/project/repo))' dune-project
  $ dune build
  $ cat foo.opam | grep -i gitlab
  homepage: "https://gitlab.com/organization/project/repo"
  bug-reports: "https://gitlab.com/organization/project/repo/-/issues"
  dev-repo: "git+https://gitlab.com/organization/project/repo.git"
