open Import
open Jbuild

type t =
  { context    : Context.t
  ; local_bins : Path.t String_map.t
  ; local_libs : Public_lib.t String_map.t
  }

let create (context : Context.t) l ~f =
  let bin_dir = Config.local_install_bin_dir ~context:context.name in
  let local_bins, local_libs =
    List.fold_left l ~init:(String_map.empty, String_map.empty)
      ~f:(fun acc x ->
        List.fold_left (f x) ~init:acc
          ~f:(fun (local_bins, local_libs) stanza ->
            match (stanza : Stanza.t) with
            | Install { section = Bin; files; _ } ->
              (List.fold_left files ~init:local_bins
                 ~f:(fun acc { Install_conf. src; dst } ->
                   let name =
                     match dst with
                     | Some s -> s
                     | None -> Filename.basename src
                   in
                   let key =
                     if Sys.win32 && Filename.extension name = ".exe" then
                       String.sub name ~pos:0 ~len:(String.length name - 4)
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
                   String_map.add acc ~key ~data:in_bin_dir),
               local_libs)
            | Library { public = Some pub; _ } ->
              (local_bins,
               String_map.add local_libs ~key:pub.name ~data:pub)
            | _ ->
              (local_bins, local_libs)))
  in
  { context
  ; local_bins
  ; local_libs
  }

let binary t ?hint name =
  if not (Filename.is_relative name) then
    Ok (Path.absolute name)
  else
    match String_map.find name t.local_bins with
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
          }

let file_of_lib t ~from ~lib ~file =
  match String_map.find lib t.local_libs with
  | Some { package; sub_dir; _ } ->
    let lib_install_dir =
      Config.local_install_lib_dir ~context:t.context.name ~package:package.name
    in
    let lib_install_dir =
      match sub_dir with
      | None -> lib_install_dir
      | Some dir -> Path.relative lib_install_dir dir
    in
    Ok (Path.relative lib_install_dir file)
  | None ->
    match
      Findlib.find t.context.findlib lib
        ~required_by:[With_required_by.Entry.jbuild_file_in ~dir:from]
    with
    | Ok pkg ->
      Ok (Path.relative (Findlib.Package.dir pkg) file)
    | Error na ->
      Error { fail = fun () ->
        raise (Findlib.Findlib (Package_not_available na)) }
