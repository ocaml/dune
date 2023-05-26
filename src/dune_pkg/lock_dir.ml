open Import
open Dune_lang

module Source = struct
  type t =
    | External_copy of Loc.t * Path.External.t
    | Fetch of
        { url : Loc.t * string
        ; checksum : (Loc.t * Checksum.t) option
        }

  module Fields = struct
    let copy = "copy"

    let fetch = "fetch"

    let url = "url"

    let checksum = "checksum"
  end

  let decode =
    let open Dune_lang.Decoder in
    sum
      [ ( Fields.copy
        , located string >>| fun (loc, source) path ->
          External_copy
            ( loc
            , if Filename.is_relative source then
                Path.External.relative path source
              else Path.External.of_string source ) )
      ; ( Fields.fetch
        , enter @@ fields
          @@ let+ url = field Fields.url (located string)
             and+ checksum = field_o Fields.checksum (located string) in
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

  let encode t =
    let open Dune_lang.Encoder in
    match t with
    | External_copy (_loc, path) ->
      constr Fields.copy string (Path.External.to_string path)
    | Fetch { url = _loc, url; checksum } ->
      record
        [ (Fields.url, string url)
        ; ( Fields.checksum
          , (option Checksum.encode) (Option.map checksum ~f:snd) )
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

  let op_by_string =
    [ ("=", OpamParserTypes.Eq)
    ; ("+=", PlusEq)
    ; ("=+", EqPlus)
    ; (":=", ColonEq)
    ; ("=:", EqColon)
    ; ("=+=", EqPlusEq)
    ]

  let decode =
    let open Dune_lang.Decoder in
    let env_update_op = enum op_by_string in
    let+ op, var, value = triple env_update_op string String_with_vars.decode in
    { op; var; value }

  let encode { op; var; value } =
    let open Dune_lang.Encoder in
    let env_update_op = enum op_by_string in
    triple env_update_op string String_with_vars.encode (op, var, value)
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

  module Fields = struct
    let version = "version"

    let install = "install"

    let build = "build"

    let deps = "deps"

    let source = "source"

    let dev = "dev"

    let exported_env = "exported_env"
  end

  let decode =
    let open Dune_lang.Decoder in
    enter @@ fields
    @@ let+ version = field ~default:"dev" Fields.version string
       and+ install_command = field_o Fields.install Dune_lang.Action.decode_pkg
       and+ build_command = field_o Fields.build Dune_lang.Action.decode_pkg
       and+ deps = field ~default:[] Fields.deps (repeat Package_name.decode)
       and+ source = field_o Fields.source Source.decode
       and+ dev = field_b Fields.dev
       and+ exported_env =
         field Fields.exported_env ~default:[] (repeat Env_update.decode)
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

  let encode
      { build_command
      ; install_command
      ; deps
      ; info = { Pkg_info.name = _; version; dev; source }
      ; lock_dir = _
      ; exported_env
      } =
    let open Dune_lang.Encoder in
    record_fields
      [ field Fields.version string version
      ; field_o Fields.install Dune_lang.Action.encode install_command
      ; field_o Fields.build Dune_lang.Action.encode build_command
      ; field_l Fields.deps Package_name.encode deps
      ; field_o Fields.source Source.encode source
      ; field_b Fields.dev dev
      ; field_l Fields.exported_env Env_update.encode exported_env
      ]
end

type t =
  { version : Syntax.Version.t
  ; packages : Pkg.t Package_name.Map.t
  }

let create_latest_version packages =
  let version = Syntax.greatest_supported_version Dune_lang.Pkg.syntax in
  { version; packages }

let path = Path.Source.(relative root "dune.lock")

let metadata = "lock.dune"

module Metadata = Dune_sexp.Versioned_file.Make (Unit)

let () = Metadata.Lang.register Dune_lang.Pkg.syntax ()

let encode_metadata t =
  let open Dune_lang.Encoder in
  list sexp
    [ string "lang"
    ; string (Syntax.name Dune_lang.Pkg.syntax)
    ; Dune_lang.Syntax.Version.encode t.version
    ]

let file_contents_by_path t =
  (metadata, [ encode_metadata t ])
  :: (Package_name.Map.to_list t.packages
     |> List.map ~f:(fun (name, pkg) ->
            (Package_name.to_string name, Pkg.encode pkg)))

let write_disk ~lock_dir_path t =
  let lock_dir_path = Path.source lock_dir_path in
  Path.rm_rf lock_dir_path;
  Path.mkdir_p lock_dir_path;
  file_contents_by_path t
  |> List.iter ~f:(fun (path_within_lock_dir, contents) ->
         let path = Path.relative lock_dir_path path_within_lock_dir in
         Option.iter (Path.parent path) ~f:Path.mkdir_p;
         let contents_string =
           List.map contents ~f:Dune_lang.to_string |> String.concat ~sep:"\n"
         in
         Io.write_file path contents_string)
