Expected: The dependency "inter_lib" in ./dune file was printed, the "inter_lib" lib is already
an internal lib that declared in ./lib/dune file. The command "dune describe external-lib-deps"
print only the external libraries by dir.

  $ dune describe external-lib-deps
  (default
   ((library
     ((names (foo))
      (package ())
      (source_dir .)
      (external_deps ((a required)))))
    (library
     ((names (inter_lib))
      (package ())
      (source_dir lib)
      (external_deps ((a required)))))))
