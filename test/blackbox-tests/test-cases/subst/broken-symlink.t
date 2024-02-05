dune subst should not fail when encountering broken symlinks.
See #9593.

  $ cat > dune-project << EOF
  > (lang dune 1.0)
  > (name proj)
  > (package
  >  (name proj))
  > EOF
  $ ln -s nonexistent broken 

This test requires a git repository, otherwise `dune subst` does nothing.

  $ git init -q
  $ git add dune-project broken
  $ git commit -m create |grep -v root-commit
   2 files changed, 5 insertions(+)
   create mode 120000 broken
   create mode 100644 dune-project

  $ dune subst
