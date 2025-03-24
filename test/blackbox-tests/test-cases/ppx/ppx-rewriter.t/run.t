  $ dune build ./w_omp_driver.exe
  -arg: omp

This test is broken because ppx_driver doesn't support migrate custom arguments
#  $ dune build ./w_ppx_driver_flags.exe
  $ dune build && dune exec -- ocamlfind opt -package fooppx -ppxopt "fooppx,-flag" -linkpkg w_omp_driver.ml -o w_omp_driver.exe
  pass -arg to fooppx
  -arg: 
