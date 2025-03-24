Expected: The dependency "inter_lib" in ./dune file was printed, the "inter_lib" lib is already
an internal lib that declared in ./lib/dune file. The command "dune describe external-lib-deps"
print only the external libraries by dir.

  $ dune describe external-lib-deps
  (default
   ((library
     ((names (inter_lib))
      (extensions ())
      (package ())
      (source_dir lib)
      (external_deps ((a required)))
      (internal_deps ())))
    (library
     ((names (foo))
      (extensions ())
      (package ())
      (source_dir .)
      (external_deps ((a required)))
      (internal_deps ((inter_lib required)))))))
