Test 'source' stanza compatibility with both user and organization paths from
the supported 'github', 'gitlab', 'sourcehut', and 'bitbucket'.

Test a generated 'github' user repo

  $ cat > dune-project <<EOF
  > (lang dune 3.21)
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

  $ sed -i -e '4s|.*|(source (gitlab user/repo))|' dune-project
  $ dune build
  $ cat foo.opam | grep -i gitlab
  homepage: "https://gitlab.com/user/repo"
  bug-reports: "https://gitlab.com/user/repo/-/issues"
  dev-repo: "git+https://gitlab.com/user/repo.git"

Test a generated 'sourcehut' user repo

  $ sed -i -e '4s|.*|(source (sourcehut user/repo))|' dune-project
  $ dune build
  $ cat foo.opam | grep -i sr.ht
  homepage: "https://sr.ht/~user/repo"
  bug-reports: "https://todo.sr.ht/~user/repo"
  dev-repo: "git+https://git.sr.ht/~user/repo"

Test a generated 'bitbucket' user repo

  $ sed -i -e '4s|.*|(source (bitbucket user/repo))|' dune-project
  $ dune build
  $ cat foo.opam | grep -i bitbucket
  homepage: "https://bitbucket.org/user/repo"
  bug-reports: "https://bitbucket.org/user/repo/issues"
  dev-repo: "git+https://bitbucket.org/user/repo.git"

Test a generated 'gitlab' organization repo

  $ sed -i -e '4s|.*|(source (gitlab organization/project/repo))|' dune-project
  $ dune build
  $ cat foo.opam | grep -i gitlab
  homepage: "https://gitlab.com/organization/project/repo"
  bug-reports: "https://gitlab.com/organization/project/repo/-/issues"
  dev-repo: "git+https://gitlab.com/organization/project/repo.git"

Test a generated 'codeberg' user repo

  $ sed -i -e '4s|.*|(source (codeberg user/repo))|' dune-project
  $ dune build
  $ cat foo.opam | grep -i codeberg.org
  homepage: "https://codeberg.org/user/repo"
  bug-reports: "https://codeberg.org/user/repo/issues"
  dev-repo: "git+https://codeberg.org/user/repo.git"

Test a generated 'tangled' user repo

  $ sed -i -e '4s|.*|(source (tangled @user.domain/repo))|' dune-project
  $ dune build
  $ cat foo.opam | grep -i tangled.org
  homepage: "https://tangled.org/@user.domain/repo"
  bug-reports: "https://tangled.org/@user.domain/repo/issues"
  dev-repo: "git+https://tangled.org/@user.domain/repo"

Test that the creation of a source stanza of the form 'org/project/repo' is
disallowed by any forge type other than gitlab and that associated error
messages are provided

Test github forge.

  $ sed -i -e '4s|.*|(source (github org/proj/repo))|' dune-project
  $ dune build
  File "dune-project", line 4, characters 16-29:
  4 | (source (github org/proj/repo))
                      ^^^^^^^^^^^^^
  Error: Github repository must be of form user/repo
  Hint: The provided form 'org/proj/repo' is specific to GitLab projects
  [1]

Test bitbucket forge.

  $ sed -i -e '4s|.*|(source (bitbucket org/proj/repo))|' dune-project
  $ dune build
  File "dune-project", line 4, characters 19-32:
  4 | (source (bitbucket org/proj/repo))
                         ^^^^^^^^^^^^^
  Error: Bitbucket repository must be of form user/repo
  Hint: The provided form 'org/proj/repo' is specific to Gitlab projects
  [1]

Test sourcehut forge.

  $ sed -i -e '4s|.*|(source (sourcehut org/proj/repo))|' dune-project
  $ dune build
  File "dune-project", line 4, characters 19-32:
  4 | (source (sourcehut org/proj/repo))
                         ^^^^^^^^^^^^^
  Error: Sourcehut repository must be of form user/repo
  Hint: The provided form 'org/proj/repo' is specific to Gitlab projects
  [1]

Test codeberg forge.

  $ sed -i -e '4s|.*|(source (codeberg org/proj/repo))|' dune-project
  $ dune build
  File "dune-project", line 4, characters 18-31:
  4 | (source (codeberg org/proj/repo))
                        ^^^^^^^^^^^^^
  Error: Codeberg repository must be of form user/repo
  Hint: The provided form 'org/proj/repo' is specific to Gitlab projects
  [1]

So far we have been using '(lang dune 3.17)' which supports gitlab organization
style syntax, we will bump the version down and check to make sure an error is
thrown telling us we need a more recent version of dune to use orginaziton
syntax.

  $ sed -i -e '1s|.*|(lang dune 3.16)|' dune-project
  $ sed -i -e '4s|.*|(source (gitlab org/proj/repo))|' dune-project
  $ dune build
  File "dune-project", line 4, characters 8-30:
  4 | (source (gitlab org/proj/repo))
              ^^^^^^^^^^^^^^^^^^^^^^
  Error: Gitlab organization repo is only available since version 3.17 of the
  dune language. Please update your dune-project file to have (lang dune 3.17).
  [1]

With the version bumped down we will also check to make sure that the user/repo
style gitlab stanza still works without any error.

  $ sed -i -e '4s|.*|(source (gitlab user/repo))|' dune-project
  $ dune build
