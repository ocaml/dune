external library dependencies of a simple project

  $ echo "(lang dune 2.3)" > dune-project
  $ touch dummypkg.opam
  $ cat >dune <<EOF
  > (library
  >  (public_name dummypkg)
  >  (libraries base doesnotexist.foo))
  > (test
  >  (package dummypkg)
  >  (name test)
  >  (libraries base))
  > EOF
  $ dune describe external-lib-deps
  (default
   ((library
     ((names (dummypkg))
      (extensions ())
      (package (dummypkg))
      (source_dir .)
      (external_deps
       ((base required)
        (doesnotexist.foo required)))
      (internal_deps ())))
    (tests
     ((names (test))
      (extensions
       (.bc .exe))
      (package (dummypkg))
      (source_dir .)
      (external_deps ((base required)))
      (internal_deps ())))))
