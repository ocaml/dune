Demonstrate $ dune subst without a git repository

  $ cat > dune-project << EOF
  > (lang dune 3.16)
  > (name test)
  > EOF

  $ echo "%%NAME%%" > README.md

  $ dune subst

  $ cat README.md
  %%NAME%%
