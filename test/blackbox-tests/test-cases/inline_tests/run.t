  $ env -u OCAMLRUNPARAM jbuilder runtest simple -j1 --display quiet --root .
           run alias simple/runtest (exit 2)
  (cd _build/default/simple && ./.foo_simple.inline-tests/run.exe)
  Fatal error: exception File "simple/.foo_simple.inline-tests/run.ml", line 1, characters 10-16: Assertion failed
  [1]

  $ jbuilder runtest missing-backend -j1 --display quiet --root .
  File "missing-backend/jbuild", line 3, characters 2-16:
  Error: No inline tests backend found.
  [1]

  $ jbuilder runtest too-many-backends -j1 --display quiet --root .
  File "too-many-backends/jbuild", line 17, characters 2-16:
  Error: Too many independant inline tests backends found:
  - "backend_tmb1" in _build/default/too-many-backends
  - "backend_tmb2" in _build/default/too-many-backends
  [1]

  $ jbuilder runtest many-backends-choose -j1 --display quiet --root .
           run alias many-backends-choose/runtest
  backend_mbc1

  $ jbuilder runtest dune-file -j1 --display quiet --root .
  (dune
   1
   ((inline_tests.backend
     1.0
     ((runner_libraries (str))
      (flags
       (inline-test-runner
        ${library-name}
        -source-tree-root
        ${ROOT}
        -diff-cmd
        -))
      (generate_runner
       (progn
        (echo "let () = print_int 41")
        (echo "\n")
        (echo "let () = print_int 42")
        (echo "\n")
        (echo "let () = print_int 43;;")))))))
           run alias dune-file/runtest
  414243
