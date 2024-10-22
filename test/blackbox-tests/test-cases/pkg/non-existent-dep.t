A package depending on a package that doesn't exist.
The solver should give a more sane error message.

  $ cat > dune-project << EOF
  > (lang dune 3.15)
  > (name abc)
  > (package
  > (name abc)
  > (synopsis "A short synopsis")
  > (description "A longer description")
  > (depends
  >   ocaml
  >   dune
  >   base
  >   re
  >   unix
  >   foobar
  >   core
  >   core_unix
  >   ocamlformat
  >   ocamlformat-lib)
  > (tags
  >  (topics "to describe" your project)))
  > EOF

  $ dune pkg lock
  Error: Unable to solve dependencies for the following lock directories:
  Lock directory dune.lock:
  Can't find all required versions.
  Selected: abc.dev base.v0.17.1 base-threads.base base-unix.base
            base_bigstring.v0.17.0 base_quickcheck.v0.17.0 bin_prot.v0.17.0
            capitalization.v0.17.0 cmdliner.1.3.0 core.v0.17.1
            core_kernel.v0.17.0 core_unix.v0.17.0 csexp.1.5.2
            dune-configurator.3.16.0 expect_test_helpers_core.v0.17.0
            fieldslib.v0.17.0 gel.v0.17.0 int_repr.v0.17.0
            jane-street-headers.v0.17.0 jst-config.v0.17.0 num.1.5-1
            ocaml.5.4.0 ocaml-config.3 ocaml_intrinsics_kernel.v0.17.1
            ocamlformat.0.26.2 parsexp.v0.17.0 ppx_assert.v0.17.0
            ppx_base.v0.17.0 ppx_bench.v0.17.0 ppx_cold.v0.17.0
            ppx_compare.v0.17.0 ppx_custom_printf.v0.17.0 ppx_diff.v0.17.0
            ppx_disable_unused_warnings.v0.17.0 ppx_enumerate.v0.17.0
            ppx_expect.v0.17.2 ppx_fields_conv.v0.17.0
            ppx_fixed_literal.v0.17.0 ppx_hash.v0.17.0 ppx_here.v0.17.0
            ppx_ignore_instrumentation.v0.17.0 ppx_inline_test.v0.17.0
            ppx_jane.v0.17.0 ppx_let.v0.17.0 ppx_log.v0.17.0
            ppx_module_timer.v0.17.0 ppx_optcomp.v0.17.0 ppx_optional.v0.17.0
            ppx_pipebang.v0.17.0 ppx_sexp_conv.v0.17.0 ppx_sexp_message.v0.17.0
            ppx_sexp_value.v0.17.0 ppx_stable.v0.17.0
            ppx_stable_witness.v0.17.0 ppx_string.v0.17.0
            ppx_string_conv.v0.17.0 ppx_tydi.v0.17.0 ppx_typerep_conv.v0.17.0
            ppx_variants_conv.v0.17.0 ppxlib_jane.v0.17.1 re.1.12.0 seq.base
            sexp_pretty.v0.17.0 sexplib.v0.17.0 sexplib0.v0.17.0 spawn.v0.15.1
            splittable_random.v0.17.0 stdio.v0.17.0 time_now.v0.17.0
            timezone.v0.17.0 uopt.v0.17.0 variantslib.v0.17.0
            ocaml-base-compiler ocaml-base-compiler ocaml
  - dune -> dune.3.16.0
      User requested = 3.17
  - foobar -> (problem)
      No known implementations at all
  - ocaml-base-compiler -> (problem)
      Rejected candidates:
        ocaml-base-compiler.5.2.0: Requires ocaml = 5.2.0
        ocaml-base-compiler.5.1.1: Requires ocaml = 5.1.1
        ocaml-base-compiler.5.1.0: Requires ocaml = 5.1.0
        ocaml-base-compiler.5.0.0: Requires ocaml = 5.0.0
        ocaml-base-compiler.4.14.2: Requires ocaml = 4.14.2
        ...
  - ocamlformat-lib -> (problem)
      ocamlformat 0.26.2 requires = 0.26.2
      Rejected candidates:
        ocamlformat-lib.0.26.2: Requires ocaml >= 4.08 & < 5.3
        ocamlformat-lib.0.26.1: Incompatible with restriction: = 0.26.2
        ocamlformat-lib.0.26.0: Incompatible with restriction: = 0.26.2
        ocamlformat-lib.0.25.1: Incompatible with restriction: = 0.26.2
  - ppx_bin_prot -> (problem)
      Rejected candidates:
        ppx_bin_prot.v0.17.0: Requires ppxlib_jane >= v0.17 & < v0.17.1
        ppx_bin_prot.v0.16.0: Requires base >= v0.16 & < v0.17
        ppx_bin_prot.v0.15.0: Requires base >= v0.15 & < v0.16
        ppx_bin_prot.v0.14.0: Requires base >= v0.14 & < v0.15
        ppx_bin_prot.v0.13.0: Requires base >= v0.13 & < v0.14
        ...
  - ppx_globalize -> (problem)
      ppx_base v0.17.0 requires >= v0.17 & < v0.18
      Rejected candidates:
        ppx_globalize.v0.17.0: Requires ocaml >= 5.1.0 & < 5.3
        ppx_globalize.v0.16.0: Incompatible with restriction: >= v0.17 & <
  v0.18
  - ppxlib -> (problem)
      base_quickcheck v0.17.0 requires >= 0.28.0
      Rejected candidates:
        ppxlib.0.33.0: Requires ocaml >= 4.04.1 & < 5.3.0
        ppxlib.0.32.1: Requires ocaml >= 4.04.1 & < 5.3.0
        ppxlib.0.32.0: Requires ocaml >= 4.04.1 & < 5.2.0 & <> 5.1.0~alpha1
        ppxlib.0.31.0: Requires ocaml >= 4.04.1 & < 5.2.0 & <> 5.1.0~alpha1
        ppxlib.0.30.0: Requires ocaml >= 4.04.1 & < 5.2.0 & <> 5.1.0~alpha1
        ...
  - typerep -> (problem)
      core v0.17.1 requires >= v0.17 & < v0.18
      Rejected candidates:
        typerep.v0.17.0: Requires ocaml >= 5.1.0 & < 5.3
        typerep.v0.16.0: Incompatible with restriction: >= v0.17 & < v0.18
        typerep.v0.15.0: Incompatible with restriction: >= v0.17 & < v0.18
        typerep.v0.14.0: Incompatible with restriction: >= v0.17 & < v0.18
        typerep.v0.13.0: Incompatible with restriction: >= v0.17 & < v0.18
        ...
  - unix -> (problem)
      No known implementations at all
  [1]
The problem with unix and foobar seems important enough to not show the wall of text above...
