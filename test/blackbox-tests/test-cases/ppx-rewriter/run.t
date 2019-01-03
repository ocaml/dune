  $ dune build ./w_omp_driver.exe --display short
      ocamldep ppx/.fooppx.objs/fooppx.ml.d
        ocamlc ppx/.fooppx.objs/byte/fooppx.{cmi,cmo,cmt}
      ocamlopt ppx/.fooppx.objs/native/fooppx.{cmx,o}
      ocamlopt ppx/fooppx.{a,cmxa}
      ocamlopt .ppx/jbuild/a0597253d899c1b15660d5431f244d21/ppx.exe
           ppx w_omp_driver.pp.ml
      ocamldep .w_omp_driver.eobjs/w_omp_driver.pp.ml.d
        ocamlc .w_omp_driver.eobjs/byte/w_omp_driver.{cmi,cmo,cmt}
      ocamlopt .w_omp_driver.eobjs/native/w_omp_driver.{cmx,o}
      ocamlopt w_omp_driver.exe

This test is broken because ppx_driver doesn't support migrate custom arguments
#  $ dune build ./w_ppx_driver_flags.exe --display short
  $ dune build && dune exec -- ocamlfind opt -package fooppx -ppxopt "fooppx,-flag" -linkpkg w_omp_driver.ml -o w_omp_driver.exe
  pass -arg to fooppx
