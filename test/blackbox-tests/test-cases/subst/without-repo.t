Demonstrate $ dune subst without a git repository

  $ cat > dune-project << EOF
  > (lang dune 3.17)
  > (name test)
  > EOF

  $ echo "%%NAME%%" > README.md

  $ dune subst

  $ cat README.md
  test

dune subst should take the project version from the dune-project file when
there is no vcs

  $ cat > dune-project << EOF
  > (lang dune 3.19)
  > (name test)
  > (version 0.1)
  > EOF

  $ echo "%%VERSION%%" > README.md

  $ dune subst

  $ cat README.md
  0.1
