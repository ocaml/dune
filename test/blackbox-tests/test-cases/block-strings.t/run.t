  $ dune build @old
  ARFLAGS=rsc
  CXX=g++
  CXXFLAGS="-Wall -O3 -g -pthread"
  if ! ${.ARCH_SIXTYFOUR}; then
    CXX="$CXX -m32"
  fi
  ${.MAKE} -s -C libre2 clean
  ${.MAKE} -s -C libre2 \
    ARFLAGS="$ARFLAGS" \
    CXX="$CXX" \
    CXXFLAGS="$CXXFLAGS" \
    obj/libre2.a obj/so/libre2.so
  cp libre2/obj/libre2.a libre2_c_stubs.a
  cp libre2/obj/so/libre2.so dllre2_c_stubs.so
  ${.MAKE} -s -C libre2 clean

  $ dune build @new
  ARFLAGS=rsc
  CXX=g++
  CXXFLAGS="-Wall -O3 -g -pthread"
  if ! ${.ARCH_SIXTYFOUR}; then
    CXX="$CXX -m32"
  fi
  ${.MAKE} -s -C libre2 clean
  ${.MAKE} -s -C libre2 \
    ARFLAGS="$ARFLAGS" \
    CXX="$CXX" \
    CXXFLAGS="$CXXFLAGS" \
    obj/libre2.a obj/so/libre2.so
  cp libre2/obj/libre2.a libre2_c_stubs.a
  cp libre2/obj/so/libre2.so dllre2_c_stubs.so
  ${.MAKE} -s -C libre2 clean

  $ dune build @quoting-test
  normal: A
  raw:    \065
