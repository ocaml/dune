Exercise the solver on a package with a conjunction in its dependency
constraints.

  $ . ./helpers.sh
  $ mkrepo

  $ mkpkg a

  $ mkpkg foo << EOF
  > depends: [
  >   "a" { >= "0.0.0" & < "1.0.0" }
  > ]
  > EOF

  $ solve foo
  Solution for .dune-solution-cache:
  - a.0.0.1
  - foo.0.0.1

  $ cat ${default_lock_dir}/foo.pkg
  (version 0.0.1)
  
  (depends a)
