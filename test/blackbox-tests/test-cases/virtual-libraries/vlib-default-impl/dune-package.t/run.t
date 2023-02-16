Check that default implementation data is installed in the dune package file.

  $ dune build @install
  $ dune_cmd cat _build/install/default/lib/a/dune-package | sed "s/(lang dune .*)/(lang dune <version>)/" | dune_cmd sanitize
  (lang dune <version>)
  (name a)
  (sections (lib .) (libexec .))
  (files
   (lib
    (META
     a.cmi
     a.cmo
     a.cmt
     a.cmx
     a.ml
     a$ext_obj
     a__X.cmi
     a__X.cmti
     default-impl/a.cmi
     default-impl/a.cmx
     default-impl/a__X.cmi
     default-impl/a__X.cmt
     default-impl/a__X.cmx
     default-impl/a__a_default__.cmi
     default-impl/a__a_default__.cmt
     default-impl/a__a_default__.cmx
     default-impl/a__a_default__.ml
     default-impl/a_default$ext_lib
     default-impl/a_default.cma
     default-impl/a_default.cmxa
     default-impl/x.ml
     dune-package
     opam
     x.mli))
   (libexec (default-impl/a_default.cmxs)))
  (library
   (name a)
   (kind normal)
   (virtual)
   (default_implementation a.default-impl)
   (main_module_name A)
   (modes byte native)
   (modules
    (wrapped
     (group
      (alias
       (obj_name a)
       (visibility public)
       (kind alias)
       (source (path A) (impl (path a.ml-gen))))
      (name A)
      (modules
       (module
        (obj_name a__X)
        (visibility public)
        (kind virtual)
        (source (path X) (intf (path x.mli))))))
     (wrapped true))))
  (library
   (name a.default-impl)
   (kind normal)
   (archives
    (byte default-impl/a_default.cma)
    (native default-impl/a_default.cmxa))
   (plugins
    (byte default-impl/a_default.cma)
    (native default-impl/a_default.cmxs))
   (native_archives default-impl/a_default$ext_lib)
   (requires a)
   (implements a)
   (main_module_name A)
   (modes byte native)
   (modules
    (wrapped
     (group
      (alias
       (obj_name a__a_default__)
       (visibility public)
       (kind alias)
       (source
        (path A__a_default__)
        (impl (path default-impl/a__a_default__.ml-gen))))
      (name A)
      (modules
       (module
        (obj_name a__X)
        (visibility public)
        (kind impl_vmodule)
        (source (path X) (impl (path default-impl/x.ml))))))
     (wrapped true))))
