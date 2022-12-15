Include variants and implementation information in dune-package
  $ dune build | sed "s/(lang dune .*)/(lang dune <version>)/" | dune_cmd sanitize
  (lang dune <version>)
  (name foo)
  (sections (lib .) (libexec .))
  (files
   (lib
    (META
     dune-package
     impl/impl$ext_lib
     impl/impl.cma
     impl/impl.cmxa
     impl/vlib.cmi
     impl/vlib.cmx
     impl/vlib__Vmod.cmi
     impl/vlib__Vmod.cmt
     impl/vlib__Vmod.cmx
     impl/vlib__impl__.cmi
     impl/vlib__impl__.cmt
     impl/vlib__impl__.cmx
     impl/vlib__impl__.ml
     impl/vmod.ml
     opam
     vlib/vlib.cmi
     vlib/vlib.cmo
     vlib/vlib.cmt
     vlib/vlib.cmx
     vlib/vlib.ml
     vlib/vlib$ext_obj
     vlib/vlib__Vmod.cmi
     vlib/vlib__Vmod.cmti
     vlib/vmod.mli))
   (libexec (impl/impl.cmxs)))
  (library
   (name foo.impl)
   (kind normal)
   (archives (byte impl/impl.cma) (native impl/impl.cmxa))
   (plugins (byte impl/impl.cma) (native impl/impl.cmxs))
   (native_archives impl/impl$ext_lib)
   (requires foo.vlib)
   (implements foo.vlib)
   (main_module_name Vlib)
   (modes byte native)
   (modules
    (wrapped
     (group
      (alias
       (name Vlib__impl__)
       (obj_name vlib__impl__)
       (path Vlib__impl__)
       (visibility public)
       (kind alias)
       (impl))
      (name Vlib)
      (modules
       (module
        (name Vmod)
        (obj_name vlib__Vmod)
        (path Vmod)
        (visibility public)
        (kind impl_vmodule)
        (impl))))
     (wrapped true))))
  (library
   (name foo.vlib)
   (kind normal)
   (virtual)
   (main_module_name Vlib)
   (modes byte native)
   (modules
    (wrapped
     (group
      (alias
       (name Vlib)
       (obj_name vlib)
       (path Vlib)
       (visibility public)
       (kind alias)
       (impl))
      (name Vlib)
      (modules
       (module
        (name Vmod)
        (obj_name vlib__Vmod)
        (path Vmod)
        (visibility public)
        (kind virtual)
        (intf))))
     (wrapped true))))
