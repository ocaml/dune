  $ . ./helpers.sh
  $ mkrepo

Testing the output of the dune describe pkg lock command.

First we setup a repo.
  $ mkpkg A 1.2.0
  > mkpkg B 2.1+rc1
  > mkpkg C 81.0.4044.138 <<EOF
  > depends: [ "D" "E" ]
  > EOF
  > mkpkg D 0.4.0.beta1
  > mkpkg E 3.0~alpha1

  $ cat > dune-workspace <<EOF
  > (lang dune 3.11)
  > (context
  >  (default))
  > (context
  >  (default
  >   (name "foo")
  >   (lock_dir foo.lock)))
  > (lock_dir
  >  (repositories mock))
  > (lock_dir
  >  (path foo.lock)
  >  (repositories mock))
  > (repository
  >  (name mock)
  >  (source "file://$(pwd)/mock-opam-repository"))
  > EOF

Here is the output of solving for multiple contexts:
  $ solve_project --all <<EOF
  > (lang dune 3.11)
  > (package
  >  (name x)
  >  (depends A B C))
  > EOF
  Solution for dune.lock:
  - A.1.2.0
  - B.2.1+rc1
  - C.81.0.4044.138
  - D.0.4.0.beta1
  - E.3.0~alpha1
  Solution for foo.lock:
  - A.1.2.0
  - B.2.1+rc1
  - C.81.0.4044.138
  - D.0.4.0.beta1
  - E.3.0~alpha1
Here is the output of dune describe pkg lock:
  $ dune describe pkg lock
  Contents of dune.lock:
  - A.1.2.0
  - B.2.1+rc1
  - C.81.0.4044.138
  - D.0.4.0.beta1
  - E.3.0~alpha1

The names of the lockfiles can also be provided:
  $ dune describe pkg lock dune.lock foo.lock 
  Contents of dune.lock:
  - A.1.2.0
  - B.2.1+rc1
  - C.81.0.4044.138
  - D.0.4.0.beta1
  - E.3.0~alpha1
  Contents of foo.lock:
  - A.1.2.0
  - B.2.1+rc1
  - C.81.0.4044.138
  - D.0.4.0.beta1
  - E.3.0~alpha1
