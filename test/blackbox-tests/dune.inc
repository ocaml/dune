(alias
 ((name aliases)
  (deps ((package dune) (files_recursively_in test-cases/aliases)))
  (action
   (chdir
    test-cases/aliases
    (progn (run ${exe:cram.exe} -test run.t) (diff? run.t run.t.corrected))))))

(alias
 ((name bad-alias-error)
  (deps ((package dune) (files_recursively_in test-cases/bad-alias-error)))
  (action
   (chdir
    test-cases/bad-alias-error
    (progn (run ${exe:cram.exe} -test run.t) (diff? run.t run.t.corrected))))))

(alias
 ((name byte-code-only)
  (deps ((package dune) (files_recursively_in test-cases/byte-code-only)))
  (action
   (chdir
    test-cases/byte-code-only
    (progn (run ${exe:cram.exe} -test run.t) (diff? run.t run.t.corrected))))))

(alias
 ((name c-stubs)
  (deps ((package dune) (files_recursively_in test-cases/c-stubs)))
  (action
   (chdir
    test-cases/c-stubs
    (progn (run ${exe:cram.exe} -test run.t) (diff? run.t run.t.corrected))))))

(alias
 ((name configurator)
  (deps ((package dune) (files_recursively_in test-cases/configurator)))
  (action
   (chdir
    test-cases/configurator
    (progn
     (run ${exe:cram.exe} -skip-platforms win -test run.t)
     (diff? run.t run.t.corrected))))))

(alias
 ((name copy_files)
  (deps ((package dune) (files_recursively_in test-cases/copy_files)))
  (action
   (chdir
    test-cases/copy_files
    (progn (run ${exe:cram.exe} -test run.t) (diff? run.t run.t.corrected))))))

(alias
 ((name cross-compilation)
  (deps ((package dune) (files_recursively_in test-cases/cross-compilation)))
  (action
   (chdir
    test-cases/cross-compilation
    (progn (run ${exe:cram.exe} -test run.t) (diff? run.t run.t.corrected))))))

(alias
 ((name depend-on-the-universe)
  (deps
   ((package dune) (files_recursively_in test-cases/depend-on-the-universe)))
  (action
   (chdir
    test-cases/depend-on-the-universe
    (progn (run ${exe:cram.exe} -test run.t) (diff? run.t run.t.corrected))))))

(alias
 ((name env)
  (deps ((package dune) (files_recursively_in test-cases/env)))
  (action
   (chdir
    test-cases/env
    (progn (run ${exe:cram.exe} -test run.t) (diff? run.t run.t.corrected))))))

(alias
 ((name exclude-missing-module)
  (deps
   ((package dune) (files_recursively_in test-cases/exclude-missing-module)))
  (action
   (chdir
    test-cases/exclude-missing-module
    (progn (run ${exe:cram.exe} -test run.t) (diff? run.t run.t.corrected))))))

(alias
 ((name exec-cmd)
  (deps ((package dune) (files_recursively_in test-cases/exec-cmd)))
  (action
   (chdir
    test-cases/exec-cmd
    (progn (run ${exe:cram.exe} -test run.t) (diff? run.t run.t.corrected))))))

(alias
 ((name findlib)
  (deps ((package dune) (files_recursively_in test-cases/findlib)))
  (action
   (chdir
    test-cases/findlib
    (progn (run ${exe:cram.exe} -test run.t) (diff? run.t run.t.corrected))))))

(alias
 ((name force-test)
  (deps ((package dune) (files_recursively_in test-cases/force-test)))
  (action
   (chdir
    test-cases/force-test
    (progn (run ${exe:cram.exe} -test run.t) (diff? run.t run.t.corrected))))))

(alias
 ((name gen-opam-install-file)
  (deps
   ((package dune) (files_recursively_in test-cases/gen-opam-install-file)))
  (action
   (chdir
    test-cases/gen-opam-install-file
    (progn (run ${exe:cram.exe} -test run.t) (diff? run.t run.t.corrected))))))

(alias
 ((name github20)
  (deps ((package dune) (files_recursively_in test-cases/github20)))
  (action
   (chdir
    test-cases/github20
    (progn (run ${exe:cram.exe} -test run.t) (diff? run.t run.t.corrected))))))

(alias
 ((name github24)
  (deps ((package dune) (files_recursively_in test-cases/github24)))
  (action
   (chdir
    test-cases/github24
    (progn (run ${exe:cram.exe} -test run.t) (diff? run.t run.t.corrected))))))

(alias
 ((name github25)
  (deps ((package dune) (files_recursively_in test-cases/github25)))
  (action
   (setenv
    OCAMLPATH
    ./findlib-packages
    (chdir
     test-cases/github25
     (progn (run ${exe:cram.exe} -test run.t) (diff? run.t run.t.corrected)))))))

(alias
 ((name github534)
  (deps ((package dune) (files_recursively_in test-cases/github534)))
  (action
   (chdir
    test-cases/github534
    (progn (run ${exe:cram.exe} -test run.t) (diff? run.t run.t.corrected))))))

(alias
 ((name github568)
  (deps ((package dune) (files_recursively_in test-cases/github568)))
  (action
   (chdir
    test-cases/github568
    (progn (run ${exe:cram.exe} -test run.t) (diff? run.t run.t.corrected))))))

(alias
 ((name github597)
  (deps ((package dune) (files_recursively_in test-cases/github597)))
  (action
   (chdir
    test-cases/github597
    (progn (run ${exe:cram.exe} -test run.t) (diff? run.t run.t.corrected))))))

(alias
 ((name github644)
  (deps ((package dune) (files_recursively_in test-cases/github644)))
  (action
   (chdir
    test-cases/github644
    (progn (run ${exe:cram.exe} -test run.t) (diff? run.t run.t.corrected))))))

(alias
 ((name github660)
  (deps ((package dune) (files_recursively_in test-cases/github660)))
  (action
   (chdir
    test-cases/github660
    (progn (run ${exe:cram.exe} -test run.t) (diff? run.t run.t.corrected))))))

(alias
 ((name github717-odoc-index)
  (deps
   ((package dune) (files_recursively_in test-cases/github717-odoc-index)))
  (action
   (chdir
    test-cases/github717-odoc-index
    (progn
     (run ${exe:cram.exe} -skip-versions 4.02.3 -test run.t)
     (diff? run.t run.t.corrected))))))

(alias
 ((name github734)
  (deps ((package dune) (files_recursively_in test-cases/github734)))
  (action
   (chdir
    test-cases/github734
    (progn (run ${exe:cram.exe} -test run.t) (diff? run.t run.t.corrected))))))

(alias
 ((name github759)
  (deps ((package dune) (files_recursively_in test-cases/github759)))
  (action
   (chdir
    test-cases/github759
    (progn (run ${exe:cram.exe} -test run.t) (diff? run.t run.t.corrected))))))

(alias
 ((name github761)
  (deps ((package dune) (files_recursively_in test-cases/github761)))
  (action
   (chdir
    test-cases/github761
    (progn (run ${exe:cram.exe} -test run.t) (diff? run.t run.t.corrected))))))

(alias
 ((name github764)
  (deps ((package dune) (files_recursively_in test-cases/github764)))
  (action
   (chdir
    test-cases/github764
    (progn
     (run ${exe:cram.exe} -skip-platforms win -test run.t)
     (diff? run.t run.t.corrected))))))

(alias
 ((name github784)
  (deps ((package dune) (files_recursively_in test-cases/github784)))
  (action
   (chdir
    test-cases/github784
    (progn (run ${exe:cram.exe} -test run.t) (diff? run.t run.t.corrected))))))

(alias
 ((name ignored_subdirs)
  (deps ((package dune) (files_recursively_in test-cases/ignored_subdirs)))
  (action
   (chdir
    test-cases/ignored_subdirs
    (progn (run ${exe:cram.exe} -test run.t) (diff? run.t run.t.corrected))))))

(alias
 ((name include-loop)
  (deps ((package dune) (files_recursively_in test-cases/include-loop)))
  (action
   (chdir
    test-cases/include-loop
    (progn (run ${exe:cram.exe} -test run.t) (diff? run.t run.t.corrected))))))

(alias
 ((name inline_tests)
  (deps ((package dune) (files_recursively_in test-cases/inline_tests)))
  (action
   (chdir
    test-cases/inline_tests
    (progn (run ${exe:cram.exe} -test run.t) (diff? run.t run.t.corrected))))))

(alias
 ((name installable-dup-private-libs)
  (deps
   ((package dune)
    (files_recursively_in test-cases/installable-dup-private-libs)))
  (action
   (chdir
    test-cases/installable-dup-private-libs
    (progn (run ${exe:cram.exe} -test run.t) (diff? run.t run.t.corrected))))))

(alias
 ((name intf-only)
  (deps ((package dune) (files_recursively_in test-cases/intf-only)))
  (action
   (chdir
    test-cases/intf-only
    (progn (run ${exe:cram.exe} -test run.t) (diff? run.t run.t.corrected))))))

(alias
 ((name js_of_ocaml)
  (deps ((package dune) (files_recursively_in test-cases/js_of_ocaml)))
  (action
   (setenv
    NODE
    ${bin:node}
    (chdir
     test-cases/js_of_ocaml
     (progn (run ${exe:cram.exe} -test run.t) (diff? run.t run.t.corrected)))))))

(alias
 ((name lib-available)
  (deps ((package dune) (files_recursively_in test-cases/lib-available)))
  (action
   (chdir
    test-cases/lib-available
    (progn (run ${exe:cram.exe} -test run.t) (diff? run.t run.t.corrected))))))

(alias
 ((name loop)
  (deps ((package dune) (files_recursively_in test-cases/loop)))
  (action
   (chdir
    test-cases/loop
    (progn (run ${exe:cram.exe} -test run.t) (diff? run.t run.t.corrected))))))

(alias
 ((name menhir)
  (deps ((package dune) (files_recursively_in test-cases/menhir)))
  (action
   (chdir
    test-cases/menhir
    (progn (run ${exe:cram.exe} -test run.t) (diff? run.t run.t.corrected))))))

(alias
 ((name merlin-tests)
  (deps ((package dune) (files_recursively_in test-cases/merlin-tests)))
  (action
   (chdir
    test-cases/merlin-tests
    (progn (run ${exe:cram.exe} -test run.t) (diff? run.t run.t.corrected))))))

(alias
 ((name meta-gen)
  (deps ((package dune) (files_recursively_in test-cases/meta-gen)))
  (action
   (chdir
    test-cases/meta-gen
    (progn (run ${exe:cram.exe} -test run.t) (diff? run.t run.t.corrected))))))

(alias
 ((name misc)
  (deps ((package dune) (files_recursively_in test-cases/misc)))
  (action
   (chdir
    test-cases/misc
    (progn (run ${exe:cram.exe} -test run.t) (diff? run.t run.t.corrected))))))

(alias
 ((name multiple-private-libs)
  (deps
   ((package dune) (files_recursively_in test-cases/multiple-private-libs)))
  (action
   (chdir
    test-cases/multiple-private-libs
    (progn
     (run ${exe:cram.exe} -skip-versions 4.02.3 -test run.t)
     (diff? run.t run.t.corrected))))))

(alias
 ((name null-dep)
  (deps ((package dune) (files_recursively_in test-cases/null-dep)))
  (action
   (chdir
    test-cases/null-dep
    (progn (run ${exe:cram.exe} -test run.t) (diff? run.t run.t.corrected))))))

(alias
 ((name ocaml-syntax)
  (deps ((package dune) (files_recursively_in test-cases/ocaml-syntax)))
  (action
   (chdir
    test-cases/ocaml-syntax
    (progn (run ${exe:cram.exe} -test run.t) (diff? run.t run.t.corrected))))))

(alias
 ((name ocamldep-multi-stanzas)
  (deps
   ((package dune) (files_recursively_in test-cases/ocamldep-multi-stanzas)))
  (action
   (chdir
    test-cases/ocamldep-multi-stanzas
    (progn (run ${exe:cram.exe} -test run.t) (diff? run.t run.t.corrected))))))

(alias
 ((name odoc)
  (deps ((package dune) (files_recursively_in test-cases/odoc)))
  (action
   (chdir
    test-cases/odoc
    (progn
     (run ${exe:cram.exe} -skip-versions 4.02.3 -test run.t)
     (diff? run.t run.t.corrected))))))

(alias
 ((name odoc-unique-mlds)
  (deps ((package dune) (files_recursively_in test-cases/odoc-unique-mlds)))
  (action
   (chdir
    test-cases/odoc-unique-mlds
    (progn
     (run ${exe:cram.exe} -skip-versions 4.02.3 -test run.t)
     (diff? run.t run.t.corrected))))))

(alias
 ((name output-obj)
  (deps ((package dune) (files_recursively_in test-cases/output-obj)))
  (action
   (chdir
    test-cases/output-obj
    (progn
     (run
      ${exe:cram.exe}
      -skip-versions
      <4.06.0
      -skip-platforms
      macosx,win
      -test
      run.t)
     (diff? run.t run.t.corrected))))))

(alias
 ((name package-dep)
  (deps ((package dune) (files_recursively_in test-cases/package-dep)))
  (action
   (chdir
    test-cases/package-dep
    (progn (run ${exe:cram.exe} -test run.t) (diff? run.t run.t.corrected))))))

(alias
 ((name ppx-rewriter)
  (deps ((package dune) (files_recursively_in test-cases/ppx-rewriter)))
  (action
   (chdir
    test-cases/ppx-rewriter
    (progn
     (run ${exe:cram.exe} -skip-versions 4.02.3 -test run.t)
     (diff? run.t run.t.corrected))))))

(alias
 ((name private-public-overlap)
  (deps
   ((package dune) (files_recursively_in test-cases/private-public-overlap)))
  (action
   (chdir
    test-cases/private-public-overlap
    (progn (run ${exe:cram.exe} -test run.t) (diff? run.t run.t.corrected))))))

(alias
 ((name promote)
  (deps ((package dune) (files_recursively_in test-cases/promote)))
  (action
   (chdir
    test-cases/promote
    (progn (run ${exe:cram.exe} -test run.t) (diff? run.t run.t.corrected))))))

(alias
 ((name quoting)
  (deps ((package dune) (files_recursively_in test-cases/quoting)))
  (action
   (chdir
    test-cases/quoting
    (progn (run ${exe:cram.exe} -test run.t) (diff? run.t run.t.corrected))))))

(alias
 ((name reason)
  (deps ((package dune) (files_recursively_in test-cases/reason)))
  (action
   (chdir
    test-cases/reason
    (progn (run ${exe:cram.exe} -test run.t) (diff? run.t run.t.corrected))))))

(alias
 ((name redirections)
  (deps ((package dune) (files_recursively_in test-cases/redirections)))
  (action
   (chdir
    test-cases/redirections
    (progn (run ${exe:cram.exe} -test run.t) (diff? run.t run.t.corrected))))))

(alias
 ((name scope-bug)
  (deps ((package dune) (files_recursively_in test-cases/scope-bug)))
  (action
   (chdir
    test-cases/scope-bug
    (progn (run ${exe:cram.exe} -test run.t) (diff? run.t run.t.corrected))))))

(alias
 ((name scope-ppx-bug)
  (deps ((package dune) (files_recursively_in test-cases/scope-ppx-bug)))
  (action
   (chdir
    test-cases/scope-ppx-bug
    (progn (run ${exe:cram.exe} -test run.t) (diff? run.t run.t.corrected))))))

(alias
 ((name select)
  (deps ((package dune) (files_recursively_in test-cases/select)))
  (action
   (chdir
    test-cases/select
    (progn (run ${exe:cram.exe} -test run.t) (diff? run.t run.t.corrected))))))

(alias
 ((name utop)
  (deps ((package dune) (files_recursively_in test-cases/utop)))
  (action
   (chdir
    test-cases/utop
    (progn (run ${exe:cram.exe} -test run.t) (diff? run.t run.t.corrected))))))

(alias
 ((name runtest)
  (deps
   ((alias aliases)
    (alias bad-alias-error)
    (alias byte-code-only)
    (alias c-stubs)
    (alias configurator)
    (alias copy_files)
    (alias cross-compilation)
    (alias depend-on-the-universe)
    (alias env)
    (alias exclude-missing-module)
    (alias exec-cmd)
    (alias findlib)
    (alias force-test)
    (alias gen-opam-install-file)
    (alias github20)
    (alias github24)
    (alias github25)
    (alias github534)
    (alias github568)
    (alias github597)
    (alias github644)
    (alias github660)
    (alias github717-odoc-index)
    (alias github734)
    (alias github759)
    (alias github761)
    (alias github764)
    (alias github784)
    (alias ignored_subdirs)
    (alias include-loop)
    (alias inline_tests)
    (alias installable-dup-private-libs)
    (alias intf-only)
    (alias lib-available)
    (alias loop)
    (alias menhir)
    (alias merlin-tests)
    (alias meta-gen)
    (alias misc)
    (alias multiple-private-libs)
    (alias null-dep)
    (alias ocaml-syntax)
    (alias ocamldep-multi-stanzas)
    (alias odoc)
    (alias odoc-unique-mlds)
    (alias output-obj)
    (alias package-dep)
    (alias ppx-rewriter)
    (alias private-public-overlap)
    (alias promote)
    (alias quoting)
    (alias redirections)
    (alias scope-bug)
    (alias scope-ppx-bug)
    (alias select)
    (alias utop)))))

(alias
 ((name runtest-no-deps)
  (deps
   ((alias aliases)
    (alias bad-alias-error)
    (alias byte-code-only)
    (alias c-stubs)
    (alias configurator)
    (alias copy_files)
    (alias cross-compilation)
    (alias depend-on-the-universe)
    (alias env)
    (alias exclude-missing-module)
    (alias exec-cmd)
    (alias findlib)
    (alias force-test)
    (alias gen-opam-install-file)
    (alias github20)
    (alias github24)
    (alias github25)
    (alias github534)
    (alias github568)
    (alias github597)
    (alias github660)
    (alias github734)
    (alias github759)
    (alias github761)
    (alias github764)
    (alias github784)
    (alias ignored_subdirs)
    (alias include-loop)
    (alias inline_tests)
    (alias installable-dup-private-libs)
    (alias intf-only)
    (alias lib-available)
    (alias loop)
    (alias merlin-tests)
    (alias meta-gen)
    (alias misc)
    (alias null-dep)
    (alias ocaml-syntax)
    (alias ocamldep-multi-stanzas)
    (alias output-obj)
    (alias package-dep)
    (alias promote)
    (alias quoting)
    (alias redirections)
    (alias scope-bug)
    (alias scope-ppx-bug)
    (alias select)))))

(alias ((name runtest-disabled) (deps ((alias reason)))))

(alias ((name runtest-js) (deps ((alias js_of_ocaml)))))