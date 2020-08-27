open Import

let () = Hooks.End_of_build.always Dune_util.Report_error.clear_reported

let report (e : Exn_with_backtrace.t) =
  let exn, dependency_path = Dep_path.unwrap_exn e.exn in
  let extra (loc : Loc.t option) =
    let dependency_path =
      let dependency_path = Option.value dependency_path ~default:[] in
      if !Clflags.debug_dep_path then
        dependency_path
      else
        (* Only keep the part that doesn't come from the build system *)
        let rec drop : Dep_path.Entries.t -> _ = function
          | (Path _ | Alias _) :: l -> drop l
          | l -> l
        in
        match loc with
        | None -> drop dependency_path
        | Some loc ->
          if Filename.is_relative loc.start.pos_fname then
            (* If the error points to a local file, no need to print the
               dependency stack *)
            []
          else
            drop dependency_path
    in
    if dependency_path <> [] then
      Some (Dep_path.Entries.pp (List.rev dependency_path))
    else
      None
  in
  Dune_util.Report_error.report ~extra { e with exn }
