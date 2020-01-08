Show commands run by Dune.
All calls to `odoc compile` and `odoc html` should have the `--warn-error` option.

  $ dune build @doc --display verbose 2>&1 | \
  >  sed "s#$(opam var prefix)/#%OPAM_PREFIX%/#" | \
  >  grep -v "Running\[.*\]: .*/ocamlc.opt -config > /tmp/dune.*\.output"
  # Workspace root: $TESTCASE_ROOT
  # disable binary cache
  # Dune context:
  #  { name = "default"
  #  ; kind = "default"
  #  ; profile = Dyn
  #  ; merlin = true
  #  ; for_host = None
  #  ; fdo_target_exe = None
  #  ; build_dir = "default"
  #  ;
  #  toplevel_path = Some External "%OPAM_PREFIX%/lib/toplevel"
  #  ; ocaml_bin = External "%OPAM_PREFIX%/bin"
  #  ; ocaml = External "%OPAM_PREFIX%/bin/ocaml"
  #  ; ocamlc = External "%OPAM_PREFIX%/bin/ocamlc.opt"
  #  ; ocamlopt = Some External "%OPAM_PREFIX%/bin/ocamlopt.opt"
  #  ; ocamldep = External "%OPAM_PREFIX%/bin/ocamldep.opt"
  #  ; ocamlmklib = External "%OPAM_PREFIX%/bin/ocamlmklib.opt"
  #  ; env = map {}
  #  ;
  #  findlib_path =
  #    [ External "/home/jules/w/dune/_build/install/default/lib"
  #    ; External "%OPAM_PREFIX%/lib"
  #    ]
  #  ; arch_sixtyfour = true
  #  ; natdynlink_supported = true
  #  ; supports_shared_libraries = true
  #  ; opam_vars = map {}
  #  ;
  #  ocaml_config =
  #    { version = "4.08.1"
  #    ; standard_library_default = "%OPAM_PREFIX%/lib/ocaml"
  #    ; standard_library = "%OPAM_PREFIX%/lib/ocaml"
  #    ; standard_runtime = "the_standard_runtime_variable_was_deleted"
  #    ; ccomp_type = "cc"
  #    ; c_compiler = "gcc"
  #    ; ocamlc_cflags = [ "-O2"; "-fno-strict-aliasing"; "-fwrapv"; "-fPIC" ]
  #    ; ocamlopt_cflags = [ "-O2"; "-fno-strict-aliasing"; "-fwrapv" ]
  #    ;
  #    bytecomp_c_compiler =
  #      [ "gcc"
  #      ; "-O2"
  #      ; "-fno-strict-aliasing"
  #      ; "-fwrapv"
  #      ; "-fPIC"
  #      ; "-D_FILE_OFFSET_BITS=64"
  #      ; "-D_REENTRANT"
  #      ]
  #    ; bytecomp_c_libraries = [ "-lm"; "-ldl"; "-lpthread" ]
  #    ;
  #    native_c_compiler =
  #      [ "gcc"
  #      ; "-O2"
  #      ; "-fno-strict-aliasing"
  #      ; "-fwrapv"
  #      ; "-D_FILE_OFFSET_BITS=64"
  #      ; "-D_REENTRANT"
  #      ]
  #    ; native_c_libraries = [ "-lm"; "-ldl" ]
  #    ; cc_profile = [ "-pg" ]
  #    ; architecture = "amd64"
  #    ; model = "default"
  #    ; int_size = 63
  #    ; word_size = 64
  #    ; system = "linux"
  #    ; asm = [ "as" ]
  #    ; asm_cfi_supported = true
  #    ; with_frame_pointers = false
  #    ; ext_exe = ""
  #    ; ext_obj = "$ext_obj"
  #    ; ext_asm = "$ext_asm"
  #    ; ext_lib = "$ext_lib"
  #    ; ext_dll = "$ext_dll"
  #    ; os_type = "Unix"
  #    ; default_executable_name = "a.out"
  #    ; systhread_supported = true
  #    ; host = "x86_64-pc-linux-gnu"
  #    ; target = "x86_64-pc-linux-gnu"
  #    ; profiling = true
  #    ; flambda = false
  #    ; spacetime = false
  #    ; safe_string = false
  #    ; exec_magic_number = "Caml1999X025"
  #    ; cmi_magic_number = "Caml1999I025"
  #    ; cmo_magic_number = "Caml1999O025"
  #    ; cma_magic_number = "Caml1999A025"
  #    ; cmx_magic_number = "Caml1999Y025"
  #    ; cmxa_magic_number = "Caml1999Z025"
  #    ; ast_impl_magic_number = "Caml1999M025"
  #    ; ast_intf_magic_number = "Caml1999N025"
  #    ; cmxs_magic_number = "Caml1999D025"
  #    ; cmt_magic_number = "Caml1999T025"
  #    ; natdynlink_supported = true
  #    ; supports_shared_libraries = true
  #    ; windows_unicode = false
  #    }
  #  ;
  #  which =
  #    map
  #      { "ocaml" : Some External "%OPAM_PREFIX%/bin/ocaml"
  #      ;
  #      "ocamlc" : Some External "%OPAM_PREFIX%/bin/ocamlc.opt"
  #      ;
  #      "ocamlobjinfo" :
  #        Some External "%OPAM_PREFIX%/bin/ocamlobjinfo.opt"
  #      }
  #  }
  # Actual targets:
  # - recursive alias @./doc
  Running[1]: (cd _build/default && %OPAM_PREFIX%/bin/ocamldep.opt -modules -intf lib/foo.mli) > _build/default/lib/.foo.objs/foo.mli.d
  Running[2]: (cd _build/default && %OPAM_PREFIX%/bin/ocamlc.opt -w @1..3@5..28@30..39@43@46..47@49..57@61..62-40 -strict-sequence -strict-formats -short-paths -keep-locs -g -bin-annot -I lib/.foo.objs/byte -no-alias-deps -opaque -o lib/.foo.objs/byte/foo.cmi -c -intf lib/foo.mli)
  Running[3]: (cd _build/default/_doc/_odoc/pkg/foo && %OPAM_PREFIX%/bin/odoc compile --pkg foo -o page-foo.odoc ../../../../doc/foo.mld)
  Running[4]: (cd _build/default/_doc/_odoc/pkg/foo && %OPAM_PREFIX%/bin/odoc compile --pkg foo -o page-index.odoc ../../../_mlds/foo/index.mld)
  Running[5]: (cd _build/default/lib/.foo.objs/byte && %OPAM_PREFIX%/bin/odoc compile --warn-error -I . -I ../../../_doc/_odoc/pkg/foo --pkg foo -o foo.odoc foo.cmti)
  Running[6]: (cd _build/default/_doc/_html && %OPAM_PREFIX%/bin/odoc html -I ../_odoc/pkg/foo -I ../../lib/.foo.objs/byte -o . ../_odoc/pkg/foo/page-foo.odoc)
  Running[7]: (cd _build/default && %OPAM_PREFIX%/bin/odoc support-files -o _doc/_html)
  Running[8]: (cd _build/default/_doc/_html && %OPAM_PREFIX%/bin/odoc html -I ../_odoc/pkg/foo -I ../../lib/.foo.objs/byte -o . ../_odoc/pkg/foo/page-index.odoc)
  Running[9]: (cd _build/default/_doc/_html && %OPAM_PREFIX%/bin/odoc html -I ../_odoc/pkg/foo -I ../../lib/.foo.objs/byte -o . ../../lib/.foo.objs/byte/foo.odoc)
