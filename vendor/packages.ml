open Import

(* Package specifications for vendor updater *)

type revision =
  | Commit of string
  | Tag of string

type source =
  | Git of
      { url : string
      ; revision : revision
      ; build_cmd : [ `Dune of string list ] option
      }

module Copy_rule = struct
  type ('src_path, 'dst_path) t =
    | Copy of
        { src : 'src_path
        ; dst_dir : 'dst_path
        }
    | Copy_rename of
        { src : 'src_path
        ; dst : 'dst_path
        }
    | Copy_glob of
        { src_dir : 'src_path
        ; pattern : string
        ; dst_dir : 'dst_path
        }
    | Remove of 'dst_path list

  let map ~(f_src : 'a -> 'c) ~(f_dst : 'b -> 'd) (rule : ('a, 'b) t) : ('c, 'd) t =
    match rule with
    | Copy { src; dst_dir } -> Copy { src = f_src src; dst_dir = f_dst dst_dir }
    | Copy_rename { src; dst } -> Copy_rename { src = f_src src; dst = f_dst dst }
    | Copy_glob { src_dir; pattern; dst_dir } ->
      Copy_glob { src_dir = f_src src_dir; pattern; dst_dir = f_dst dst_dir }
    | Remove paths -> Remove (List.map ~f:f_dst paths)
  ;;
end

type package_spec =
  { name : string
  ; source : source
  ; copy_rules : (string, string) Copy_rule.t list
  ; preserve_rules : string list
  }

let all_packages =
  [ { name = "bigstringaf"
    ; source =
        Git
          { url = "https://github.com/inhabitedtype/bigstringaf.git"
          ; revision = Commit "05d9dc328ddf1bd23e946feae16cb5c794a73fac"
          ; build_cmd = None
          }
    ; copy_rules =
        [ Copy_glob { src_dir = "lib"; pattern = "bigstringaf.{ml,mli}"; dst_dir = "." }
        ; Copy { src = "lib/bigstringaf_stubs.c"; dst_dir = "." }
        ; Copy { src = "LICENSE"; dst_dir = "." }
        ]
    ; preserve_rules = [ "dune" ]
    }
  ; { name = "build_path_prefix_map"
    ; source =
        Git
          { url = "https://gitlab.com/gasche/build_path_prefix_map.git"
          ; revision = Commit "0deb52fd3d644acb5ab3dc1db604937c71a542aa"
          ; build_cmd = None
          }
    ; copy_rules =
        [ Copy { src = "build_path_prefix_map.ml"; dst_dir = "src" }
        ; Copy { src = "build_path_prefix_map.mli"; dst_dir = "src" }
        ]
    ; preserve_rules = [ "src/dune" ]
    }
  ; { name = "cmdliner"
    ; source =
        Git
          { url = "https://github.com/ocaml-dune/cmdliner.git"
          ; revision = Commit "430664dd13c4645bda0e9c39f7d0ec75c1534874"
          ; build_cmd = Some (`Dune [ "subst" ])
          }
    ; copy_rules =
        [ Copy { src = "LICENSE.md"; dst_dir = "." }
        ; Copy_glob { src_dir = "src"; pattern = "*.{ml,mli}"; dst_dir = "src" }
        ; Remove [ "src/cmdliner_exit.ml"; "src/cmdliner_exit.mli" ]
        ]
    ; preserve_rules = [ "src/dune" ]
    }
  ; { name = "csexp"
    ; source =
        Git
          { url = "https://github.com/ocaml-dune/csexp.git"
          ; revision = Commit "07eb8988452ad51a09d0ab7379d73a87674aba6e"
          ; build_cmd = None
          }
    ; copy_rules =
        [ Copy { src = "LICENSE.md"; dst_dir = "." }
        ; Copy_glob { src_dir = "src"; pattern = "*.{ml,mli}"; dst_dir = "src" }
        ]
    ; preserve_rules = []
    }
  ; { name = "incremental-cycles"
    ; source =
        Git
          { url = "https://gitlab.inria.fr/agueneau/incremental-cycles.git"
          ; revision = Commit "1e2030a5d5183d84561cde142eecca40e03db2a3"
          ; build_cmd =
              Some
                (`Dune
                    [ "build"
                    ; "--root"
                    ; "."
                    ; "export/incremental_cycles.ml"
                    ; "--verbose"
                    ])
          }
    ; copy_rules =
        [ Copy { src = "LICENSE"; dst_dir = "." }
        ; Copy { src = "README.md"; dst_dir = "." }
        ; Copy { src = "export/incremental_cycles_intf.ml"; dst_dir = "src" }
        ; Copy { src = "export/incremental_cycles.mli"; dst_dir = "src" }
        ; Copy { src = "_build/default/export/incremental_cycles.ml"; dst_dir = "src" }
        ]
    ; preserve_rules = [ "src/dune"; "README.md" ]
    }
  ; { name = "lwd"
    ; source =
        Git
          { url = "https://github.com/let-def/lwd.git"
          ; revision = Commit "3c446b45b2d9e81bc72b57ada168fe7923f9b02c"
          ; build_cmd = None
          }
    ; copy_rules =
        [ Copy { src = "LICENSE"; dst_dir = "." }
        ; Copy_glob { src_dir = "lib/lwd"; pattern = "lwd.{ml,mli}"; dst_dir = "lwd" }
        ; Copy_glob
            { src_dir = "lib/lwd"; pattern = "lwd_utils.{ml,mli}"; dst_dir = "lwd" }
        ; Copy_glob
            { src_dir = "lib/nottui"; pattern = "nottui.{ml,mli}"; dst_dir = "nottui" }
        ]
    ; preserve_rules = [ "lwd/dune"; "nottui/dune" ]
    }
  ; { name = "notty"
    ; source =
        Git
          { url = "https://github.com/ocaml-dune/notty.git"
          ; revision = Commit "54b14f0dc25316a5b64206c55598442ec9d43325"
          ; build_cmd = None
          }
    ; copy_rules =
        [ Copy { src = "LICENSE.md"; dst_dir = "." }
        ; Copy_glob { src_dir = "src"; pattern = "*.{ml,mli}"; dst_dir = "src" }
        ; Copy_glob
            { src_dir = "src-unix"; pattern = "*.{ml,mli,c}"; dst_dir = "src-unix" }
        ; Copy_glob
            { src_dir = "src-unix/native"; pattern = "*.c"; dst_dir = "src-unix/native" }
        ; Copy_glob
            { src_dir = "src/no-uucp"; pattern = "*.{ml,mli}"; dst_dir = "src/no-uucp" }
        ; Copy { src = "src/no-uucp/README.md"; dst_dir = "src/no-uucp" }
        ; Remove [ "src/notty_top.ml"; "src/notty_top_init.ml" ]
        ]
    ; preserve_rules = [ "src/dune"; "src-unix/dune" ]
    }
  ; { name = "ocaml-blake3-mini"
    ; source =
        Git
          { url = "https://github.com/rgrinberg/ocaml-blake3-mini.git"
          ; revision = Commit "c6aa40e5f1973c2e6b736660ce2c8dcd3b3f9c9f"
          ; build_cmd = None
          }
    ; copy_rules =
        [ Copy_glob { src_dir = "src"; pattern = "*.{ml,mli,c}"; dst_dir = "." }
        ; Copy_glob { src_dir = "vendor"; pattern = "*.{c,h,S,asm}"; dst_dir = "." }
        ; Remove [ "blake3_avx2.c"; "blake3_avx512.c"; "blake3_sse2.c"; "blake3_sse41.c" ]
        ]
    ; preserve_rules = [ "dune" ]
    }
  ; { name = "ocaml-inotify"
    ; source =
        Git
          { url = "https://github.com/ocaml-dune/ocaml-inotify.git"
          ; revision = Commit "1bfe079ddfc6bff72a3b6a8ae3c0408297b04434"
          ; build_cmd = None
          }
    ; copy_rules =
        [ Copy { src = "LICENSE.txt"; dst_dir = "." }
        ; Copy_glob { src_dir = "lib"; pattern = "*.{ml,mli,c}"; dst_dir = "src" }
        ; Remove [ "src/lwt_inotify.ml"; "src/lwt_inotify.mli" ]
        ]
    ; preserve_rules = [ "src/dune" ]
    }
  ; { name = "ocaml-lmdb"
    ; source =
        Git
          { url = "https://github.com/Drup/ocaml-lmdb.git"
          ; revision = Commit "43b466d43766bb5d75cfa05dce9efe82e4946490"
          ; build_cmd = None
          }
    ; copy_rules =
        [ Copy_glob { src_dir = "src"; pattern = "lmdb.{ml,mli}"; dst_dir = "." }
        ; Copy_glob { src_dir = "src"; pattern = "lmdb_bindings.{ml,mli}"; dst_dir = "." }
        ; Copy { src = "src/lmdb_stubs.c"; dst_dir = "." }
        ; Copy { src = "LICENSE.md"; dst_dir = "." }
        ]
    ; preserve_rules = [ "dune"; "gen_c_flags.ml" ]
    }
  ; { name = "ocaml-lmdb"
    ; source =
        Git
          { url = "https://github.com/LMDB/lmdb.git"
          ; revision = Commit "14d6629bc8a9fe40d8a6bee1bf71c45afe7576b6"
          ; build_cmd = None
          }
    ; copy_rules =
        [ Copy { src = "libraries/liblmdb/lmdb.h"; dst_dir = "." }
        ; Copy { src = "libraries/liblmdb/mdb.c"; dst_dir = "." }
        ; Copy_glob
            { src_dir = "libraries/liblmdb"; pattern = "midl.{c,h}"; dst_dir = "." }
        ; Copy { src = "libraries/liblmdb/COPYRIGHT"; dst_dir = "." }
        ; Copy_rename { src = "libraries/liblmdb/LICENSE"; dst = "LICENSE-lmdb" }
        ]
    ; preserve_rules = []
    }
  ; { name = "opam-file-format"
    ; source =
        Git
          { url = "https://github.com/ocaml-dune/opam-file-format.git"
          ; revision = Commit "426a63dadaba7f47435c9b01d8cb932c548ff2e2"
          ; build_cmd = None
          }
    ; copy_rules =
        [ Copy { src = "LICENSE"; dst_dir = "." }
        ; Copy_glob { src_dir = "src"; pattern = "*.{ml,mli,mll,mly}"; dst_dir = "." }
        ]
    ; preserve_rules = [ "dune" ]
    }
  ; { name = "opam"
    ; source =
        Git
          { url = "https://github.com/ocaml-dune/opam.git"
          ; revision = Commit "7c05922290adb214f834d9e0f1efe7028c4ebb85"
          ; build_cmd = None
          }
    ; copy_rules =
        [ Copy_glob
            { src_dir = "src/core"
            ; pattern = "*.{ml,mli,mll,mly,c}"
            ; dst_dir = "src/core"
            }
        ; Copy_glob
            { src_dir = "src/format"
            ; pattern = "*.{ml,mli,mll,mly,c}"
            ; dst_dir = "src/format"
            }
        ; Remove
            [ "src/core/opamACL.ml"
            ; "src/core/opamACL.mli"
            ; "src/core/opamCached.ml"
            ; "src/core/opamCached.mli"
            ; "src/core/opamProcess.ml"
            ; "src/core/opamProcess.mli"
            ; "src/format/opamPath.ml"
            ; "src/format/opamPath.mli"
            ]
        ]
    ; preserve_rules = [ "src/core/dune"; "src/format/dune" ]
    }
  ; { name = "pp"
    ; source =
        Git
          { url = "https://github.com/ocaml-dune/pp.git"
          ; revision = Commit "b6741dd41ef5fc5bda8b3640097ac29818a43577"
          ; build_cmd = None
          }
    ; copy_rules =
        [ Copy { src = "LICENSE.md"; dst_dir = "." }
        ; Copy_glob { src_dir = "src"; pattern = "*.{ml,mli}"; dst_dir = "src" }
        ]
    ; preserve_rules = []
    }
  ; { name = "re"
    ; source =
        Git
          { url = "https://github.com/ocaml/ocaml-re.git"
          ; revision = Tag "1.13.2"
          ; build_cmd = None
          }
    ; copy_rules =
        [ Copy { src = "LICENSE.md"; dst_dir = "." }
        ; Copy_glob { src_dir = "lib"; pattern = "*.{ml,mli}"; dst_dir = "src" }
        ]
    ; preserve_rules = []
    }
  ; { name = "sha"
    ; source =
        Git
          { url = "https://github.com/djs55/ocaml-sha"
          ; revision = Tag "v1.15.4"
          ; build_cmd = None
          }
    ; copy_rules =
        [ Copy_glob { src_dir = "."; pattern = "*.{mli,c,h}"; dst_dir = "." }
        ; Copy_glob { src_dir = "."; pattern = "sha*.ml"; dst_dir = "." }
        ; Copy { src = "c_flags.ml"; dst_dir = "flags" }
        ]
    ; preserve_rules = [ "dune"; "flags/dune" ]
    }
  ; { name = "spawn"
    ; source =
        Git
          { url = "https://github.com/ocaml-dune/spawn.git"
          ; revision = Commit "835c49833d41221758c4ad71f07829ee259668e1"
          ; build_cmd = None
          }
    ; copy_rules =
        [ Copy { src = "src/spawn.ml"; dst_dir = "src" }
        ; Copy { src = "src/spawn.mli"; dst_dir = "src" }
        ; Copy { src = "src/spawn_stubs.c"; dst_dir = "src" }
        ; Copy { src = "LICENSE.md"; dst_dir = "." }
        ]
    ; preserve_rules = []
    }
  ; { name = "uutf"
    ; source =
        Git
          { url = "https://github.com/dbuenzli/uutf.git"
          ; revision = Commit "e85bc867f03f5b253bf0335b829edd1e55b0e8c0"
          ; build_cmd = None
          }
    ; copy_rules =
        [ Copy { src = "LICENSE.md"; dst_dir = "." }
        ; Copy_glob { src_dir = "src"; pattern = "*.{ml,mli}"; dst_dir = "." }
        ]
    ; preserve_rules = [ "dune" ]
    }
  ]
;;
