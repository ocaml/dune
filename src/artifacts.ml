open! Stdune
open Import

module Bin = struct

  type t = {
    context : Context.t;
    (* Mapping from executable names to their actual path in the
       workspace. The keys are the executable names without the .exe,
       even on Windows. *)
    local_bins : Path.Build.t String.Map.t;
  }

  let binary t ?hint ~loc name =
    if not (Filename.is_relative name) then
      Ok (Path.of_filename_relative_to_initial_cwd name)
    else
      match String.Map.find t.local_bins name with
      | Some path -> Ok (Path.build path)
      | None ->
        match Context.which t.context name with
        | Some p -> Ok p
        | None ->
          Error (
            let context = t.context.name in
            Action.Prog.Not_found.create ~program:name ?hint ~context ~loc ())

  let add_binaries t ~dir l =
    let local_bins =
      List.fold_left l ~init:t.local_bins
        ~f:(fun acc fb ->
          let path = File_binding.Expanded.dst_path fb
                       ~dir:(Utils.local_bin dir) in
          String.Map.add acc (Path.Build.basename path) path)
    in
    { t with local_bins }

  let create ~(context : Context.t) ~local_bins =
    let local_bins =
      Path.Build.Set.fold local_bins ~init:String.Map.empty ~f:(fun path acc ->
        let name = Path.Build.basename path in
        let key =
          if Sys.win32 then
            Option.value ~default:name
              (String.drop_suffix name ~suffix:".exe")
          else
            name
        in
        String.Map.add acc key path)
    in
    { context
    ; local_bins
    }

end

module Public_libs = struct
  type t = {
    context : Context.t;
    public_libs : Lib.DB.t;
  }

  let create ~context ~public_libs = { context; public_libs; }

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
          | _  -> Path.Build.relative lib_install_dir (String.concat rest ~sep:"/")
        in
        Ok (Path.build (Path.Build.relative lib_install_dir file))
      end else
        Ok (Path.relative (Lib.src_dir lib) file)

end

type t = {
  public_libs : Public_libs.t;
  bin : Bin.t;
}

let create (context : Context.t) ~public_libs ~local_bins =
  {
    public_libs = Public_libs.create ~context ~public_libs;
    bin = Bin.create ~context ~local_bins;
  }
