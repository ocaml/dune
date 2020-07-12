  $ dune runtest dune-file | sed "s/(lang dune .*)/(lang dune <version>)/" | dune_cmd sanitize
           run alias dune-file/runtest
  414243
  (lang dune <version>)
  (name foo)
  (library
   (name foo)
   (kind normal)
   (archives (byte foo.cma) (native foo.cmxa))
   (plugins (byte foo.cma) (native foo.cmxs))
   (native_archives foo$ext_lib)
   (main_module_name Foo)
   (modes byte native)
   (modules
    (wrapped
     (main_module_name Foo)
     (alias_module
      (name Foo)
      (obj_name foo)
      (visibility public)
      (kind alias)
      (impl))
     (wrapped true)))
   (inline_tests.backend
    (runner_libraries str)
    (flags
     inline-test-runner
     %{library-name}
     -source-tree-root
     %{workspace_root}
     -diff-cmd
     -)
    (generate_runner
     (progn
      (echo "let () = print_int 41")
      (echo "\n")
      (echo "let () = print_int 42")
      (echo "\n")
      (echo "let () = print_int 43;;")))))

  $ dune build dune-file/foo.install && dune install foo --prefix install | dune_cmd sanitize
  Installing install/lib/foo/META
  Installing install/lib/foo/dune-package
  Installing install/lib/foo/foo.a
  Installing install/lib/foo/foo.cma
  Installing install/lib/foo/foo.cmi
  Installing install/lib/foo/foo.cmt
  Installing install/lib/foo/foo.cmx
  Installing install/lib/foo/foo.cmxa
  Installing install/lib/foo/foo.cmxs
  Installing install/lib/foo/foo.ml
  Installing install/lib/foo/opam

Make sure we can read generated dune-package files:

  $ export OCAMLPATH=$PWD/install/lib; dune runtest --root dune-file-user
  Entering directory 'dune-file-user'
           run alias runtest
  414243
