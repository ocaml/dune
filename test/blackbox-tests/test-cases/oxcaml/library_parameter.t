This test verifies that the `library_parameter` stanza works as expected.

  $ cat > "dune-project" <<EOF
  > (lang dune 3.20)
  > EOF

  $ echo "(package (name param))" >> "dune-project"

We create our first parameter. We use the `public_name` to prevent dune from
being lazy.

  $ mkdir param
  $ echo "type t = int" > param/param_intf.mli
  $ cat > "param/dune" <<EOF
  > (library_parameter
  >  (public_name param.intf)
  >  (name param_intf))
  > EOF

The test build should fails because the oxcaml extension is not available.

  $ dune build
  File "param/dune", lines 1-3, characters 0-64:
  1 | (library_parameter
  2 |  (public_name param.intf)
  3 |  (name param_intf))
  Error: 'library_parameter' is available only when oxcaml is enabled in the
  dune-project file. You must enable it using (using oxcaml 0.1) in your
  dune-project file.
  Note however that oxcaml is experimental and might change without notice in
  the future.
  [1]

Adding the extension in the dune project solve the problem.

  $ echo "(using oxcaml 0.1)" >> dune-project
  $ dune build
  $ ocamlobjinfo _build/default/param/.param_intf.objs/byte/param_intf.cmi | grep 'Is parameter'
  Is parameter: YES

We create a second parameter in the same directory. We rewrite the dune file to
use the `modules` field.

  $ cat > "param/dune" <<EOF
  > (library_parameter
  >  (public_name param.intf)
  >  (name param_intf)
  >  (modules param_intf))
  > (library_parameter
  >  (public_name param.intf2)
  >  (name param_intf2)
  >  (modules param_intf2))
  > EOF

  $ echo "type t = string" > param/param_intf2.mli

We rebuild it and ensure we have the new parameter available.

  $ dune build
  $ ocamlobjinfo _build/default/param/.param_intf2.objs/byte/param_intf2.cmi | grep "Is parameter"
  Is parameter: YES

We create a third parameter with multiple modules. The test should fail as we
limit the parameter to one module now.

  $ echo "(package (name multiple_param))" >> "dune-project"
  $ mkdir multiple
  $ cat > "multiple/dune" << EOF
  > (library_parameter
  >  (public_name multiple_param)
  >  (name multiple_param)
  >  (modules multiple_param data))
  > EOF
  $ cat > "multiple/data.mli" << EOF
  > type t = int
  > EOF
  $ cat > "multiple/multiple_param.mli" << EOF
  > type t = string
  > EOF

  $ dune build
  File "multiple/dune", lines 1-4, characters 0-103:
  1 | (library_parameter
  2 |  (public_name multiple_param)
  3 |  (name multiple_param)
  4 |  (modules multiple_param data))
  Error: a library_parameter must declare exactly one module.
  [1]

We make sure the same happened if multiple modules exists in one directory.

  $ cat > "multiple/dune" << EOF
  > (library_parameter
  >  (public_name multiple_param)
  >  (name multiple_param))
  > EOF
  $ dune build
  File "multiple/dune", lines 1-3, characters 0-72:
  1 | (library_parameter
  2 |  (public_name multiple_param)
  3 |  (name multiple_param))
  Error: a library_parameter must declare exactly one module.
  [1]

We build the installable version to ensure we have generated the correct
information to access the parameter through different packages.

  $ rm -rf multiple
  $ cat > "dune-project" << EOF
  > (lang dune 3.20)
  > (package (name param))
  > (using oxcaml 0.1)
  > EOF
  $ dune build @install

We check the META files exist.

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

We ensure we have access to the `dune-package` and it contains the two
parameters.

  $ cat _build/install/default/lib/param/dune-package | grep -v "lang dune"
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
   (main_module_name Param_intf)
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
   (main_module_name Param_intf2)
   (modes byte)
   (modules
    (singleton
     (obj_name param_intf2)
     (visibility public)
     (kind parameter)
     (source (path Param_intf2) (intf (path intf2/param_intf2.mli))))))

We verify it will install the data to the correct location.

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

