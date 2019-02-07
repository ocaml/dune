open Stdune

let pped_path path ~suffix =
  (* We need to insert the suffix before the extension as some tools
     inspect the extension *)
  let base, ext = Path.split_extension path in
  Path.extend_basename base ~suffix:(suffix ^ ext)

let pped_module m =
  Module.map_files m ~f:(fun _kind file ->
    let pp_path = pped_path file.path ~suffix:".pp" in
    { file with path = pp_path })

type t = (Module.t -> Module.t) Dune_file.Per_module.t

let reason_rules (m : Module.t) =
  Module.map_files m ~f:(fun _ f ->
    match f.syntax with
    | OCaml  -> f
    | Reason ->
      let path =
        let base, ext = Path.split_extension f.path in
        let suffix =
          match ext with
          | ".re"  -> ".re.ml"
          | ".rei" -> ".re.mli"
          | _     ->
            Errors.fail
              (Loc.in_file (Path.drop_build_context_exn f.path))
              "Unknown file extension for reason source file: %S"
              ext
        in
        Path.extend_basename base ~suffix
      in
      Module.File.make OCaml path)

let make preprocess =
  Dune_file.Per_module.map preprocess ~f:(function
    | Dune_file.Preprocess.No_preprocessing -> reason_rules
    | Action (_, _) ->
      fun m -> reason_rules (pped_module m)
    | Pps { loc = _; pps = _; flags = _; staged } ->
      if staged then
        reason_rules
      else
        fun m -> pped_module (reason_rules m))

let pped_modules (t : t) modules =
  Module.Name.Map.map modules ~f:(fun (m : Module.t) ->
    Dune_file.Per_module.get t (Module.name m) m)
