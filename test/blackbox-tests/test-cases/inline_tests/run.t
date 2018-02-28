  $ $JBUILDER runtest simple -j1 --display quiet --root .
           run alias simple/runtest (exit 2)
  (cd _build/default/simple && ./.foo_simple.inline-tests/run.exe)
  Fatal error: exception File "simple/.foo_simple.inline-tests/run.ml", line 1, characters 10-16: Assertion failed
  [1]

  $ $JBUILDER runtest missing-backend -j1 --display quiet --root .
  File "missing-backend/jbuild", line 3, characters 2-16:
  Error: No inline tests backend found.
  [1]

  $ $JBUILDER runtest too-many-backends -j1 --display quiet --root .
  File "too-many-backends/jbuild", line 17, characters 2-16:
  Error: Too many independant inline tests backends found:
  - "backend_tmb1" in _build/default/too-many-backends
  - "backend_tmb2" in _build/default/too-many-backends
  [1]

  $ $JBUILDER runtest many-backends-choose -j1 --display quiet --root .
           run alias many-backends-choose/runtest
  backend_mbc1
