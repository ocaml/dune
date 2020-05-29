  $ cat >dune-project <<EOF
  > (lang dune 2.5)
  > EOF

  $ cat >dune <<EOF
  > (library
  >  (name hello)
  >  (bisect_ppx))
  > EOF

  $ cat >dune-workspace <<EOF
  > (lang dune 2.6)
  > 
  > (context default)
  > (context (default (name coverage) (bisect_enabled true)))
  > EOF

  $ dune build @all 2>&1 | grep -v 'file "'
  Internal error, please report upstream including the contents of _build/log.
  Description:
    ("Syntax identifier is unset",
    { name = "bisect_ppx"
    ; supported_versions = map { 1 : map { 0 : (2, 6) } }
    ; context =
        map
          { "dune" : (2, 5)
          ; "action-plugin" : (0, 0)
          ; "fmt" : (1, 2)
          ; "dune-project" :
              { name = Anonymous "."
              ; root = "."
              ; version = None
              ; info =
                  { source = None
                  ; license = None
                  ; homepage = None
                  ; documentation = None
                  ; bug_reports = None
                  ; maintainers = None
                  ; authors = None
                  }
              ; project_file =
                  { file = "dune-project"
                  ; exists = true
                  ; project_name = Anonymous "."
                  }
              ; packages = []
              ; implicit_transitive_deps = true
              ; wrapped_executables = true
              ; dune_version = (2, 5)
              ; allow_approx_merlin = false
              ; generate_opam_files = false
              ; file_key = "26bb1931b3ad"
              ; dialects =
                  map
                    { "ocaml" :
                        { name = "ocaml"
                        ; file_kinds =
                            { impl =
                                { kind = "impl"
                                ; extension = ".ml"
                                ; preprocess = None
                                ; format =
                                    Some
                                      ([ "chdir"
                                       ; template "%{workspace_root}"
                                       ; [ "run"
                                         ; "ocamlformat"
                                         ; "--impl"
                                         ; template "%{input-file}"
                                         ]
                                       ],
                                      [ ".ocamlformat"
                                      ; ".ocamlformat-ignore"
                                      ; ".ocamlformat-enable"
                                      ])
                                }
                            ; intf =
                                { kind = "intf"
                                ; extension = ".mli"
                                ; preprocess = None
                                ; format =
                                    Some
                                      ([ "chdir"
                                       ; template "%{workspace_root}"
                                       ; [ "run"
                                         ; "ocamlformat"
                                         ; "--intf"
                                         ; template "%{input-file}"
                                         ]
                                       ],
                                      [ ".ocamlformat"
                                      ; ".ocamlformat-ignore"
                                      ; ".ocamlformat-enable"
                                      ])
                                }
                            }
                        }
                    ; "reason" :
                        { name = "reason"
                        ; file_kinds =
                            { impl =
                                { kind = "impl"
                                ; extension = ".re"
                                ; preprocess =
                                    Some
                                      [ "run"
                                      ; "refmt"
                                      ; "--print"
                                      ; "binary"
                                      ; template "%{input-file}"
                                      ]
                                ; format =
                                    Some
                                      ([ "run"
                                       ; "refmt"
                                       ; template "%{input-file}"
                                       ],
                                      [])
                                }
                            ; intf =
                                { kind = "intf"
                                ; extension = ".rei"
                                ; preprocess =
                                    Some
                                      [ "run"
                                      ; "refmt"
                                      ; "--print"
                                      ; "binary"
                                      ; template "%{input-file}"
                                      ]
                                ; format =
                                    Some
                                      ([ "run"
                                       ; "refmt"
                                       ; template "%{input-file}"
                                       ],
                                      [])
                                }
                            }
                        }
                    }
              ; explicit_js_mode = true
              ; format_config = Some { enabled_for = "all" }
              ; strict_package_deps = false
              }
          ; "menhir" : (2, 1)
          ; "experimental_building_ocaml_compiler_with_dune" : (0, 0)
          ; "coq" : (0, 2)
          ; "dune-bootstrap-info" : (0, 1)
          ; "mdx" : (0, 1)
          ; "cinaps" : (1, 0)
          }
    })
    17-28
    22-51
  
  I must not crash.  Uncertainty is the mind-killer. Exceptions are the
  little-death that brings total obliteration.  I will fully express my cases. 
  Execution will pass over me and through me.  And when it has gone past, I
  will unwind the stack along its path.  Where the cases are handled there will
  be nothing.  Only I will remain.
