open! Stdune
open Import
open Dune_file

type t =
  { context     : Context.t
  ; local_bins  : Path.t String.Map.t
  ; public_libs : Lib.DB.t
  }

let create (context : Context.t) ~public_libs l ~f =
  let bin_dir = Config.local_install_bin_dir ~context:context.name in
  let local_bins =
    List.fold_left l ~init:String.Map.empty ~f:(fun acc x ->
      List.fold_left (f x) ~init:acc ~f:(fun local_bins stanza ->
        match (stanza : Stanza.t) with
        | Install { section = Bin; files; _ } ->
          List.fold_left files ~init:local_bins
            ~f:(fun acc { Install_conf. src; dst } ->
              let name =
                match dst with
                | Some s -> s
                | None -> Filename.basename src
              in
              let key =
                if Sys.win32 then
                  Option.value ~default:name
                    (String.drop_suffix name ~suffix:".exe")
                else
                  name
              in
              let in_bin_dir =
                let fn =
                  if Sys.win32 then
                    match Filename.extension src with
                    | ".exe" | ".bc" ->
                      if Filename.extension name <> ".exe" then
                        name ^ ".exe"
                      else
                        name
                    | _ -> name
                  else
                    name
                in
                Path.relative bin_dir fn
              in
              String.Map.add acc key in_bin_dir)
        | _ ->
          local_bins))
  in
  { context
  ; local_bins
  ; public_libs
  }

let binary t ?hint ~loc name =
  if not (Filename.is_relative name) then
    Ok (Path.of_filename_relative_to_initial_cwd name)
  else
    match String.Map.find t.local_bins name with
    | Some path -> Ok path
    | None ->
      match Context.which t.context name with
      | Some p -> Ok p
      | None ->
        Error
          { Action.Prog.Not_found.
            program = name
          ; hint
          ; context = t.context.Context.name
          ; loc
          }

let file_of_lib t ~loc ~lib ~file =
  match Lib.DB.find t.public_libs lib with
  | Error reason ->
    Error { fail = fun () ->
      Lib.not_available ~loc reason "Public library %a" Lib_name.pp_quoted lib }
  | Ok lib ->
    if Lib.is_local lib then begin
      let (package, rest) = Lib_name.split (Lib.name lib) in
      let lib_install_dir =
        Config.local_install_lib_dir ~context:t.context.name ~package
      in
      let lib_install_dir =
        match rest with
        | [] -> lib_install_dir
        | _  -> Path.relative lib_install_dir (String.concat rest ~sep:"/")
      in
      Ok (Path.relative lib_install_dir file)
    end else
      Ok (Path.relative (Lib.src_dir lib) file)
