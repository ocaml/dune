open Import

let syntax =
  Syntax.create
    ~name:"oxcaml"
    ~desc:"experimental support for OxCaml"
    ~experimental:true
    [ (0, 1), `Since (3, 20) ]
;;

let extension =
  Dune_project.Extension.register syntax (Dune_lang.Decoder.return ((), [])) Dyn.unit
;;

let check_extension project =
  match Dune_project.find_extension_args project extension with
  | Some () -> ()
  | None ->
    let error =
      [ Pp.text
          "OxCaml variables are an experimental feature. They are not supported by \
           default."
      ]
    in
    let hints = [ Pp.text "Add (using oxcaml 0.1) to your dune-project" ] in
    User_error.raise ~hints error
;;

let expand project macro (ocaml : Ocaml_toolchain.t) =
  let s = Pform.Macro_invocation.Args.whole macro in
  check_extension project;
  match s with
  | "supported" -> [ Dune_lang.Value.of_bool (Ocaml_toolchain.is_oxcaml_supported ocaml) ]
  | _ -> Code_error.raise "Unknown name was request for oxcaml" [ "name", Dyn.string s ]
;;
