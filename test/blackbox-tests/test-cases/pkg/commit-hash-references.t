What happens if a branch has the same format as a ref?

  $ . ./helpers.sh
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

  $ cat > dune-workspace <<EOF
  > (lang dune 3.10)
  > (lock_dir
  >  (repositories mock))
  > (repository
  >  (name mock)
  >  (source "git+file://$PWD/mock-opam-repository#$AMBIGUOUS_REF"))
  > (context
  >  (default
  >   (name default)))
  > EOF

Depend on foo from the repo

  $ cat > dune-project <<EOF
  > (lang dune 3.10)
  > 
  > (package
  >  (name bar)
  >  (depends foo))
  > EOF

Which foo will we get?

  $ dune pkg lock 2>&1 | head -1 | sed "s/$AMBIGUOUS_REF/AMBIGUOUS_REF/g"
  revision "AMBIGUOUS_REF" not found in
