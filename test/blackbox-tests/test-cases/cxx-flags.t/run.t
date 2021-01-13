Default: use_standard_c_and_cxx_flags = false
=============================================

  $ cat >dune-project <<EOF
  > (lang dune 2.8)
  > EOF

> The flags that Dune should use
  $ GCCF="-x c++ -lstdc++ -shared-libgcc"
  $ ClangF="-x c++"
  $ MsvcF="/TP"

> Check that compiler detection is done
  $ dune build .dune/ccomp

  $ cat _build/default/.dune/ccomp |
  > grep -ce "clang\|gcc\|msvc"
  1

> No specific flags added
  $ dune rules baz.o | tr -s '\n' ' ' |
  > grep -ce "$GCCF\|$ClangF|$MsvcF"
  0
  [1]


With use_standard_c_and_cxx_flags = false
=========================================

  $ cat >dune-project <<EOF
  > (lang dune 2.8)
  > (use_standard_c_and_cxx_flags false)
  > EOF

> No specific flags added
  $ dune rules baz.o | tr -s '\n' ' ' |
  > grep -ce "$GCCF\|$ClangF|$MsvcF"
  0
  [1]

With use_standard_c_and_cxx_flags = true
========================================

  $ cat >dune-project <<EOF
  > (lang dune 2.8)
  > (use_standard_c_and_cxx_flags true)
  > EOF

> Check that compiler detection is done
  $ dune build .dune/ccomp

  $ cat _build/default/.dune/ccomp |
  > grep -ce "clang\|gcc\|msvc"
  1

> Specific flags added
  $ dune rules baz.o  | tr -s '\n' ' ' |
  > grep -ce "$GCCF\|$ClangF\|$MsvcF"
  1

  $ dune exec ./main.exe
  2046
