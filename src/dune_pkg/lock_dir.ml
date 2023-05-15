open Import
open Dune_lang

module Source = struct
  type t =
    | External_copy of Loc.t * Path.External.t
    | Fetch of
        { url : Loc.t * string
        ; checksum : (Loc.t * Checksum.t) option
        }

  let decode =
    let open Dune_lang.Decoder in
    sum
      [ ( "copy"
        , located string >>| fun (loc, source) path ->
          External_copy
            ( loc
            , if Filename.is_relative source then
                Path.External.relative path source
              else Path.External.of_string source ) )
      ; ( "fetch"
        , enter @@ fields
          @@ let+ url = field "url" (located string)
             and+ checksum = field_o "checksum" (located string) in
             let checksum =
               match checksum with
               | None -> None
               | Some ((loc, _) as checksum) -> (
                 match Checksum.of_string_user_error checksum with
                 | Ok checksum -> Some (loc, checksum)
                 | Error e -> raise (User_error.E e))
             in
             fun _ -> Fetch { url; checksum } )
      ]
end

module Pkg_info = struct
  type t =
    { name : Package_name.t
    ; version : string
    ; dev : bool
    ; source : Source.t option
    }
end

module Env_update = struct
  type 'a t =
    { op : OpamParserTypes.env_update_op
    ; var : Env.Var.t
    ; value : 'a
    }

  let decode =
    let open Dune_lang.Decoder in
    let env_update_op =
      enum
        [ ("=", OpamParserTypes.Eq)
        ; ("+=", PlusEq)
        ; ("=+", EqPlus)
        ; (":=", ColonEq)
        ; ("=:", EqColon)
        ; ("=+=", EqPlusEq)
        ]
    in
    let+ op, var, value = triple env_update_op string String_with_vars.decode in
    { op; var; value }
end

module Pkg = struct
  type t =
    { build_command : Action.t option
    ; install_command : Action.t option
    ; deps : Package_name.t list
    ; info : Pkg_info.t
    ; lock_dir : Path.Source.t
    ; exported_env : String_with_vars.t Env_update.t list
    }

  let decode =
    let open Dune_lang.Decoder in
    enter @@ fields
    @@ let+ version = field ~default:"dev" "version" string
       and+ install_command = field_o "install" Dune_lang.Action.decode_pkg
       and+ build_command = field_o "build" Dune_lang.Action.decode_pkg
       and+ deps = field ~default:[] "deps" (repeat Package_name.decode)
       and+ source = field_o "source" Source.decode
       and+ dev = field_b "dev"
       and+ exported_env =
         field "exported_env" ~default:[] (repeat Env_update.decode)
       in
       fun ~lock_dir name ->
         let info =
           let source =
             Option.map source ~f:(fun f ->
                 Path.source lock_dir |> Path.to_absolute_filename
                 |> Path.External.of_string |> f)
           in
           { Pkg_info.name; version; dev; source }
         in
         { build_command; deps; install_command; info; exported_env; lock_dir }
end

type t =
  { version : Syntax.Version.t
  ; packages : Pkg.t Package_name.Map.t
  }

let path = Path.Source.(relative root "dune.lock")

let metadata = "lock.dune"

module Metadata = Dune_sexp.Versioned_file.Make (Unit)

let () = Metadata.Lang.register Dune_lang.Pkg.syntax ()
