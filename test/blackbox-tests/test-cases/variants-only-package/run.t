Check that local variant implementations are correctly exported in the list of
known_implementations implementations when using -p

  $ dune build -p vlibfoo

  $ cat _build/install/default/lib/vlibfoo/dune-package
  (lang dune 1.10)
  (name vlibfoo)
  (library
   (name vlibfoo)
   (kind normal)
   (virtual)
   (foreign_archives (native vlibfoo$ext_lib))
   (known_implementations (somevariant implfoo))
   (main_module_name Vlibfoo)
   (modes byte native)
   (modules
    (main_module_name Vlibfoo)
    (modules
     ((name Vlibfoo)
      (obj_name vlibfoo)
      (visibility public)
      (kind virtual)
      (intf)))
    (wrapped true)))
