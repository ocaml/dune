  $ dune build @install --display short
      ocamlopt mli-only/foobar.{a,cmxa}
      ocamlopt mli-only/foobar.cmxs
        ocamlc mli-only/foobar.cma
      ocamldep mli-only/.foobar.objs/foobar.mli.d
        ocamlc mli-only/.foobar.objs/byte/foobar.{cmi,cmti}
      ocamldep impl/.foobar_impl.objs/foobar.mli.d
        ocamlc impl/.foobar_impl.objs/byte/foobar.{cmi,cmti}
      ocamldep impl/.foobar_impl.objs/foobar.ml.d
      ocamlopt impl/.foobar_impl.objs/native/foobar.{cmx,o}
      ocamlopt impl/foobar_impl.{a,cmxa}
      ocamlopt impl/foobar_impl.cmxs
        ocamlc impl/.foobar_impl.objs/byte/foobar.{cmo,cmt}
        ocamlc impl/foobar_impl.cma
  $ dune runtest
          test alias test/runtest
  testing
  $ dune install --prefix ./installed 2>&1 | grep -v "line [0-9]\+"
  Installing installed/lib/foobar/META
  Installing installed/lib/foobar/dune-package
  Installing installed/lib/foobar/foobar$ext_lib
  Installing installed/lib/foobar/foobar.cma
  Installing installed/lib/foobar/foobar.cmi
  Installing installed/lib/foobar/foobar.cmti
  Installing installed/lib/foobar/foobar.cmxa
  Installing installed/lib/foobar/foobar.cmxs
  Installing installed/lib/foobar/foobar.mli
  Installing installed/lib/foobar/impl/foobar.cmi
  Error: exception Sys_error("_build/install/default/lib/foobar/impl/foobar.cmi: No such file or directory")
  Backtrace:
  
  I must not segfault.  Uncertainty is the mind-killer.  Exceptions are
  the little-death that brings total obliteration.  I will fully express
  my cases.  Execution will pass over me and through me.  And when it
  has gone past, I will unwind the stack along its path.  Where the
  cases are handled there will be nothing.  Only I will remain.
