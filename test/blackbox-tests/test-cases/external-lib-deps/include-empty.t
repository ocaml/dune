By default, `dune describe external-lib-deps` omits libraries that have no
internal or external dependencies.

  $ cat >dune-project <<EOF
  > (lang dune 3.0)
  > EOF

  $ cat >dune <<EOF
  > (library
  >  (name empty_lib))
  > (library
  >  (name has_deps)
  >  (libraries a))
  > EOF

  $ dune describe external-lib-deps
  (default
   ((library
     ((names (has_deps))
      (extensions ())
      (package ())
      (source_dir .)
      (external_deps ((a required)))
      (internal_deps ())))))

With `--include-empty`, libraries that have no dependencies are printed too.

  $ dune describe external-lib-deps --include-empty
  (default
   ((library
     ((names (empty_lib))
      (extensions ())
      (package ())
      (source_dir .)
      (external_deps ())
      (internal_deps ())))
    (library
     ((names (has_deps))
      (extensions ())
      (package ())
      (source_dir .)
      (external_deps ((a required)))
      (internal_deps ())))))
