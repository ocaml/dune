Implementation of library from another project is allowed when explicitely
declared in the virtual library definition.

  $ dune build exe/exe.exe

  $ dune build -p vlibfoo-ext

  $ cat _build/install/default/lib/vlibfoo-ext/dune-package
  (lang dune 2.1)
  (name vlibfoo-ext)
  (library
   (name vlibfoo-ext)
   (kind normal)
   (virtual)
   (native_archives vlibfoo$ext_lib)
   (known_implementations (somevariant impl) (somevariant-2 impl-2))
   (main_module_name Vlibfoo)
   (modes byte native)
   (modules
    (singleton
     (name Vlibfoo)
     (obj_name vlibfoo)
     (visibility public)
     (kind virtual)
     (intf))))
