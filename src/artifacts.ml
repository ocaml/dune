open Import
open Jbuild

type t =
  { context    : Context.t
  ; local_bins : String_set.t
  ; local_libs : Public_lib.t String_map.t
  }

let create context stanzas =
  let local_bins, local_libs =
    List.fold_left stanzas ~init:(String_set.empty, String_map.empty)
      ~f:(fun acc (_dir, stanzas) ->
        List.fold_left stanzas ~init:acc
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
                   String_set.add name acc),
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

let binary t ?hint ?(in_the_tree=true) name =
  if not (Filename.is_relative name) then
    Ok (Path.absolute name)
  else if in_the_tree then begin
    if String_set.mem name t.local_bins then
      Ok (Path.relative (Config.local_install_bin_dir ~context:t.context.name) name)
    else
      match Context.which t.context name with
      | Some p -> Ok p
      | None ->
        Error
          { fail = fun () ->
              Utils.program_not_found name
                ~context:t.context.name
                ?hint
                ~in_the_tree:true
          }
  end else begin
    match Context.which t.context name with
    | Some p -> Ok p
    | None ->
      Error
        { fail = fun () ->
            Utils.program_not_found name
              ~context:t.context.name
              ?hint
              ~in_the_tree:false
        }
  end

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
      Findlib.find t.context.findlib lib ~required_by:[Utils.jbuild_name_in ~dir:from]
    with
    | Some pkg ->
      Ok (Path.relative pkg.dir file)
    | None ->
      Error
        { fail = fun () ->
            ignore (Findlib.find_exn t.context.findlib lib
                      ~required_by:[Utils.jbuild_name_in ~dir:from]
                    : Findlib.package);
            assert false
        }

let file_of_lib t ~from name =
  let lib, file =
    match String.lsplit2 name ~on:':' with
    | None ->
      Loc.fail (Loc.in_file (Path.to_string (Path.relative from "jbuild")))
            "invalid ${lib:...} form: %s" name
    | Some x -> x
  in
  (lib, file_of_lib t ~from ~lib ~file)
