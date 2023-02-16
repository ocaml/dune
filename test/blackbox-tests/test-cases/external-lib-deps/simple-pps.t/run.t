Expected: To get all required and pps packages

  $ dune describe external-lib-deps
  (default
   ((library
     ((names (foo))
      (package ())
      (source_dir .)
      (external_deps
       ((a________ required)
        (b________ required)
        (c________ required)
        (f________ required)
        (e________ required)
        (d________ required)))))
    (executables
     ((names (prog))
      (package ())
      (source_dir .)
      (external_deps
       ((h________ required)
        (i________ required)
        (j________ required)))))))
