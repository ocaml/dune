-std=c++11 is given on OCaml 5.0
================================

  $ cat >dune-project <<EOF
  > (lang dune 3.14)
  > EOF

  $ cat >dune <<EOF
  > ;; will fail if :standard doesn't have -std=c++11 on OCaml >= 5.0
  > (executable
  >  (name cpp11)
  >  (foreign_stubs
  >   (language cxx)
  >   (names cpp11)
  >   (flags -std=c++98 :standard)))
  > EOF

  $ (dune build ./cpp11.exe 2>/dev/null) && _build/default/cpp11.exe
  Hi from C++11
