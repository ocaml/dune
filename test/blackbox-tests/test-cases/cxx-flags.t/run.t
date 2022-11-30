Default: use_standard_c_and_cxx_flags = false
=============================================

  $ cat >dune-project <<EOF
  > (lang dune 2.8)
  > EOF

> The flags that Dune should use for compilation
  $ GCC_CF="-x c++"
  $ Clang_CF="-x c++"
  $ Msvc_CF="/TP"

> And linking
  $ GCC_LF_OPT=" -ldopt -lstdc++ -ldopt -shared-libgcc"
  $ Clang_LF_OPT=" -ldopt -lc++"
  $ Msvc_LF_OPT=""
  $ GCC_LF_LIB=" -cclib -lstdc++ -cclib -shared-libgcc"
  $ Clang_LF_LIB=" -cclib -lc++"
  $ Msvc_LF_LIB=""

> Check that compiler detection is done
  $ dune build .dune/ccomp/ccomp

  $ cat _build/default/.dune/ccomp/ccomp |
  > grep -ce "clang\|gcc\|msvc"
  1

> No specific flags added for compilation...
  $ dune rules baz.o | tr -s '\n' ' ' |
  > grep -ce "$GCC_CF\|$Clang_CF|$Msvc_CF"
  0
  [1]

  $ dune rules bazexe.o  | tr -s '\n' ' ' |
  > grep -ce "$GCC_CF\|$Clang_CF\|$Msvc_CF"
  0
  [1]

> ...nor linking
  $ dune rules libquad_stubs.a  | tr -s '\n' ' ' |
  > grep -ce "quad_stubs baz.o)"
  1

  $ dune rules main.exe  | tr -s '\n' ' ' |
  > grep -ce "Main.cmx)"
  1

With use_standard_c_and_cxx_flags = false
=========================================

  $ cat >dune-project <<EOF
  > (lang dune 2.8)
  > (use_standard_c_and_cxx_flags false)
  > EOF

> No specific flags added for compilation...
  $ dune rules baz.o | tr -s '\n' ' ' |
  > grep -ce "$GCC_CF\|$Clang_CF|$Msvc_CF"
  0
  [1]

  $ dune rules bazexe.o  | tr -s '\n' ' ' |
  > grep -ce "$GCC_CF\|$Clang_CF\|$Msvc_CF"
  0
  [1]

> ...nor linking
  $ dune rules libquad_stubs.a  | tr -s '\n' ' ' |
  > grep -ce "quad_stubs baz.o)"
  1

  $ dune rules main.exe  | tr -s '\n' ' ' |
  > grep -ce "Main.cmx)"
  1

With use_standard_c_and_cxx_flags = true
========================================

  $ cat >dune-project <<EOF
  > (lang dune 2.8)
  > (use_standard_c_and_cxx_flags true)
  > EOF

> Check that compiler detection is done
  $ dune build .dune/ccomp/ccomp

  $ cat _build/default/.dune/ccomp/ccomp |
  > grep -ce "clang\|gcc\|msvc"
  1

> Specific flags added for compilation...
  $ dune rules baz.o  | tr -s '\n' ' ' |
  > grep -ce "$GCC_CF\|$Clang_CF\|$Msvc_CF"
  1

  $ dune rules bazexe.o  | tr -s '\n' ' ' |
  > grep -ce "$GCC_CF\|$Clang_CF\|$Msvc_CF"
  1

> ..and link
  $ dune rules libquad_stubs.a  | tr -s '\n' ' ' |
  > grep -ce "quad_stubs baz.o$GCC_LF_OPT)\|quad_stubs baz.o$Clang_LF_OPT)\|quad_stubs baz.o$Msvc_LF_OPT)"
  1

  $ dune rules main.exe  | tr -s '\n' ' ' |
  > grep -ce "Main.cmx$GCC_LF_LIB)\|Main.cmx$Clang_LF_LIB)\|Main.cmx$Msvc_LF_LIB)"
  1

  $ dune clean

  $ dune exec ./main.exe
  2046
  4096
  Hello World Baz!
  Hello World Bazexe!

  $ [ -f _build/default/.dune/ccomp/ccomp ]

(this also works with sandbox=symlink, #6415)

  $ dune exec --sandbox symlink ./main.exe
  2046
  4096
  Hello World Baz!
  Hello World Bazexe!

ccomp is not computed if not required
=====================================
  $ dune clean

  $ dune exec ./sub/main_no_stubs.exe
  OK

  $ [ -f _build/default/.dune/ccomp/ccomp ]
  [1]


one can extend link flags in env
================================

  $ OTHER=" --other-flag --yet-some-other-flag)"

  $ dune rules sub/main.exe --profile some-profile  | tr -s '\n' ' ' |
  > grep -ce "Main.cmx$GCC_LF_LIB$OTHER\|Main.cmx$Clang_LF_LIB$OTHER\|Main.cmx$Msvc_LF_LIB$OTHER"
  1
