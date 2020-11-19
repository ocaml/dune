  $ cat >dune-project <<EOF
  > (lang dune 2.8)
  > EOF

  $ GCCF="-x c++ -lstdc++ -shared-libgcc"
  $ ClangF="-x c++"
  $ MsvcF="/TP"

Default: use_standard_c_and_cxx_flags = false
  $ dune rules baz.o | tr -s '\n' ' ' |
  > grep -ce "$GCCF\|$ClangF|$MsvcF"
  0
  [1]


With use_standard_c_and_cxx_flags = false
  $ cat >dune-project <<EOF
  > (lang dune 2.8)
  > (use_standard_c_and_cxx_flags false)
  > EOF

  $ dune rules baz.o | tr -s '\n' ' ' |
  > grep -ce "$GCCF\|$ClangF|$MsvcF"
  0
  [1]

With use_standard_c_and_cxx_flags = true
  $ cat >dune-project <<EOF
  > (lang dune 2.8)
  > (use_standard_c_and_cxx_flags true)
  > EOF

  $ dune rules baz.o  | tr -s '\n' ' ' |
  > grep -ce "$GCCF\|$ClangF\|$MsvcF"
  1

  $ dune exec ./main.exe
  2046
