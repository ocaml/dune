open! Import

include Coverage0

module Eval_libs = Ordered_set_lang.Make(struct
    type t = string
    let compare = String.compare
    module Map = String_map
  end)(struct
    type t = string
    type key = string
    let key x = x
  end)

module Eval_modules = Ordered_set_lang.Make(Module.Name)(struct
    type t = Module.t
    type key = Module.Name.t
    let key m = Module.name m
  end)

let lib_covered (t : Coverage0.Context.t) ~(lib : Jbuild.Library.t) =
  let covered =
    Eval_libs.eval_unordered
      ~parse:(fun ~loc:_ s -> s)
      ~standard:(
        let base = String_map.singleton lib.name lib.name in
        match lib.public with
        | None ->
          base
        | Some (p : Jbuild.Public_lib.t) ->
          String_map.add base p.name p.name
      ) t.covered_libs in
  String_map.mem covered lib.name || (match lib.public with
    | None -> false
    | Some p -> String_map.mem covered p.name)

let apply_instrumented
      (t : Coverage0.Context.t)
      ~(lib : Jbuild.Library.t)
      ~(modules : Module.t Module.Name.Map.t) =
  if lib_covered t ~lib then (
    let covered_modules =
      Eval_modules.eval_unordered
        lib.bisect.modules
        ~parse:(fun ~loc:_ s ->
          (* TODO proper handling *)
          Option.value_exn (
            Module.Name.Map.find modules (Module.Name.of_string s)
          ))
        ~standard:modules
    in
    let covered_pp : Jbuild.Preprocess.pps option Jbuild.Per_module.t =
      let pps =
        Some { Jbuild.Preprocess.
               pps = [Loc.none, Jbuild.Pp.of_string t.coverage]
             ; flags = []
             } in
      Jbuild.Per_module.of_mapping
        ~default:None
        (covered_modules
        |> Module.Name.Map.keys
        |> List.map ~f:(fun m -> ([m], pps)))
      |> function
      | Ok x -> x
      | Error _ -> failwith "TODO"
    in
    let combined_pp =
      Jbuild.Per_module.merge lib.buildable.preprocess covered_pp
        ~f:(fun _m lib_pp covered_pp ->
          match lib_pp, covered_pp with
          | pp, None -> pp
          | No_preprocessing, Some pp -> Jbuild.Preprocess.Pps pp
          | Action _, Some _ ->
            Loc.fail lib.buildable.loc "Unable to add bisect pp"
          | Pps pp, Some cpps ->
            Pps { pps = pp.pps @ cpps.pps
                ; flags = pp.flags @ cpps.flags })
        ~default:Jbuild.Preprocess.No_preprocessing
    in
    { lib with
      buildable = {lib.buildable with preprocess = combined_pp}
    }
  ) else (
    lib
  )
