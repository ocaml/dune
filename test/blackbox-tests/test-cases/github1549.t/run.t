Reproduction case for #1549: too many parentheses in installed .dune files

  $ dune build @install --root backend
  Entering directory 'backend'
  Leaving directory 'backend'

  $ dune_cmd cat backend/_build/install/default/lib/dune_inline_tests/dune-package | sed "s/(lang dune .*)/(lang dune <version>)/" | dune_cmd sanitize
  (lang dune <version>)
  (name dune_inline_tests)
  (sections (lib .) (libexec .))
  (files
   (lib
    (META
     dune-package
     opam
     simple_tests$ext_lib
     simple_tests.cma
     simple_tests.cmi
     simple_tests.cmt
     simple_tests.cmx
     simple_tests.cmxa
     simple_tests.ml))
   (libexec (simple_tests.cmxs)))
  (library
   (name dune_inline_tests)
   (kind normal)
   (archives (byte simple_tests.cma) (native simple_tests.cmxa))
   (plugins (byte simple_tests.cma) (native simple_tests.cmxs))
   (native_archives simple_tests$ext_lib)
   (main_module_name Simple_tests)
   (modes byte native)
   (modules
    (wrapped
     (main_module_name Simple_tests)
     (alias_module
      (name Simple_tests)
      (obj_name simple_tests)
      (visibility public)
      (kind alias)
      (impl))
     (wrapped true)))
   (inline_tests.backend
    (flags :standard)
    (generate_runner
     (run sed "s/(\\*TEST:\\(.*\\)\\*)/let () = \\1;;/" %{impl-files}))))

  $ env OCAMLPATH=backend/_build/install/default/lib dune runtest --root example
  Entering directory 'example'
  Leaving directory 'example'
