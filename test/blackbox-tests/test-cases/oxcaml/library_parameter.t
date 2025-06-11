
  $ cat >dune-project <<EOF
  > (lang dune 3.18)
  > (package (name param) (allow_empty))
  > (package (name mylib) (allow_empty))
  > EOF

  $ mkdir param
  $ echo 'type t = int [@@deriving show]' > param/param_intf.mli
  $ echo 'type t = int [@@deriving show]' > param/param_intf2.mli
  $ cat >param/dune <<EOF
  > (library_parameter (public_name param.intf) (name param_intf) (modules param_intf))
  > (library_parameter (public_name param.intf2) (name param_intf2) (modules param_intf2))
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

  $ ocamlobjinfo _build/default/param/.param_intf2.objs/byte/param_intf2.cmi | grep "Is parameter"
  Is parameter: YES

  $ dune build @install

  $ cat _build/install/default/lib/param/META
  package "intf" (
    directory = "intf"
    description = ""
    requires = ""
    archive(byte) = ""
    archive(native) = ""
    plugin(byte) = ""
    plugin(native) = ""
  )
  package "intf2" (
    directory = "intf2"
    description = ""
    requires = ""
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
     intf/param_intf.mli
     intf2/param_intf2.cmi
     intf2/param_intf2.cmti
     intf2/param_intf2.mli)))
  (library
   (name param.intf)
   (kind parameter)
   (modes byte)
   (modules
    (singleton
     (obj_name param_intf)
     (visibility public)
     (kind parameter)
     (source (path Param_intf) (intf (path intf/param_intf.mli))))))
  (library
   (name param.intf2)
   (kind parameter)
   (modes byte)
   (modules
    (singleton
     (obj_name param_intf2)
     (visibility public)
     (kind parameter)
     (source (path Param_intf2) (intf (path intf2/param_intf2.mli))))))

$ cat _build/install/default/lib/mylib/META
  $ cat _build/install/default/lib/mylib/dune-package
  (lang dune 3.19)
  (name mylib)
  (sections (lib .))
  (files (lib (META dune-package)))
  $ cat _build/install/default/param/
  cat: _build/install/default/param/: No such file or directory
  [1]

  $ cat _build/default/param.install
  lib: [
    "_build/install/default/lib/param/META"
    "_build/install/default/lib/param/dune-package"
    "_build/install/default/lib/param/intf/param_intf.cmi" {"intf/param_intf.cmi"}
    "_build/install/default/lib/param/intf/param_intf.cmti" {"intf/param_intf.cmti"}
    "_build/install/default/lib/param/intf/param_intf.mli" {"intf/param_intf.mli"}
    "_build/install/default/lib/param/intf2/param_intf2.cmi" {"intf2/param_intf2.cmi"}
    "_build/install/default/lib/param/intf2/param_intf2.cmti" {"intf2/param_intf2.cmti"}
    "_build/install/default/lib/param/intf2/param_intf2.mli" {"intf2/param_intf2.mli"}
  ]


