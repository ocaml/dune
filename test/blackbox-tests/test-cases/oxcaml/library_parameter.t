
  $ cat >dune-project <<EOF
  > (lang dune 3.18)
  > (package (name param))
  > (package (name mylib) (allow_empty))
  > EOF

  $ mkdir param
  $ echo 'type t = int [@@deriving show]' > param/param_intf.mli
  $ cat >param/dune <<EOF
  > (library_parameter (public_name param.intf) (name param_intf) (preprocess (pps ppx_deriving.std)))
  > EOF

$ mkdir mylib
$ echo 'type t = int' > mylib/mylib.ml
$ cat >mylib/dune <<EOF
> (library (public_name mylib))
> EOF

  $ dune build
  $ ocamlobjinfo _build/default/param/.param_intf.objs/byte/param_intf.cmi | grep 'Is parameter'
  Is parameter: YES

$ dune describe rules param/.param_intf.objs/byte/param_intf.cmi

$ /home/arthur/tarides/miniprintml/_build/default/ppml.exe _build/default/param_intf.pp.mli

  $ dune build @install

  $ cat _build/install/default/lib/param/META
  package "intf" (
    directory = "intf"
    description = ""
    requires = "ppx_deriving.runtime"
    archive(byte) = ""
    archive(native) = ""
    plugin(byte) = ""
    plugin(native) = ""
  )
  $ cat _build/install/default/lib/param/dune-package
  (lang dune 3.19)
  (name param)
  (sections (lib .))
  (files
   (lib
    (META
     dune-package
     intf/param_intf.cmi
     intf/param_intf.cmti
     intf/param_intf.mli)))
  (library
   (name param.intf)
   (kind parameter)
   (requires ppx_deriving.runtime)
   (modes byte)
   (modules
    (singleton
     (obj_name param_intf)
     (visibility public)
     (kind parameter)
     (source (path Param_intf) (intf (path intf/param_intf.mli))))))

$ cat _build/install/default/lib/mylib/META
$ cat _build/install/default/lib/mylib/dune-package

$ dune describe rules
