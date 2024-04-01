open Import
open Dune_lang.Decoder

type t =
  { exes : Executables.t
  ; locks : Locks.t
  ; package : Package.t option
  ; deps : Dep_conf.t Bindings.t
  ; enabled_if : Blang.t
  ; build_if : Blang.t
  ; action : Dune_lang.Action.t option
  }

include Stanza.Make (struct
    type nonrec t = t

    include Poly
  end)

let gen_parse names =
  fields
    (let* deps = field "deps" (Bindings.decode Dep_conf.decode) ~default:Bindings.empty in
     String_with_vars.add_user_vars_to_decoding_env
       (Bindings.var_names deps)
       (let* dune_version = Dune_lang.Syntax.get_exn Stanza.syntax in
        let+ buildable = Buildable.decode Executable
        and+ link_flags = Link_flags.Spec.decode ~check:None
        and+ names = names
        and+ package = field_o "package" Stanza_common.Pkg.decode
        and+ locks = Locks.field ()
        and+ modes =
          field
            "modes"
            Executables.Link_mode.Map.decode
            ~default:(Executables.Link_mode.Map.default_for_tests ~version:dune_version)
        and+ enabled_if = Enabled_if.decode ~allowed_vars:Any ~since:(Some (1, 4)) ()
        and+ action =
          field_o
            "action"
            (Dune_lang.Syntax.since ~fatal:false Stanza.syntax (1, 2)
             >>> Dune_lang.Action.decode_dune_file)
        and+ forbidden_libraries =
          field
            "forbidden_libraries"
            (Dune_lang.Syntax.since Stanza.syntax (2, 0)
             >>> repeat (located Lib_name.decode))
            ~default:[]
        and+ build_if =
          field
            "build_if"
            ~default:Blang.true_
            (Syntax.since Stanza.syntax (3, 9)
             >>> Enabled_if.decode_value ~allowed_vars:Any ())
        in
        { exes =
            { Executables.link_flags
            ; link_deps = []
            ; modes
            ; optional = false
            ; buildable
            ; names = Nonempty_list.of_list names |> Option.value_exn
            ; package = None
            ; promote = None
            ; install_conf = None
            ; embed_in_plugin_libraries = []
            ; forbidden_libraries
            ; bootstrap_info = None
            ; enabled_if
            ; dune_version
            }
        ; locks
        ; package
        ; deps
        ; enabled_if
        ; build_if
        ; action
        }))
;;

let multi = gen_parse (field "names" (repeat1 (located string)))
let single = gen_parse (field "name" (located string) >>| List.singleton)
