What happens if a branch has the same format as a ref?

  $ mkrepo
  $ mkpkg foo 1.0
  $ cd mock-opam-repository
  $ git init --quiet
  $ git add -A
  $ git commit --quiet -m "Initial state, foo.1.0"
  $ AMBIGUOUS_REF=c3ba68d69316351bc660679f68fdc871bfb4f2d2
  $ git switch --quiet -c $AMBIGUOUS_REF
  $ mkpkg foo 2.0
  $ git add -A
  $ git commit --quiet -m "New foo.2.0 on branch $AMBIGUOUS_REF"
  $ git switch --quiet -
  $ cd ..

Use this ref in a project

  $ add_mock_repo_if_needed "git+file://$PWD/mock-opam-repository#$AMBIGUOUS_REF"

Depend on foo from the repo

  $ cat > dune-project <<EOF
  > (lang dune 3.10)
  > 
  > (package
  >  (name bar)
  >  (depends foo))
  > EOF

Which foo will we get?

  $ dune pkg lock 2>&1 | grep "not found" | dune_cmd subst "$AMBIGUOUS_REF" '$AMBIGUOUS_REF'
  revision "$AMBIGUOUS_REF" not found in
  [1]
