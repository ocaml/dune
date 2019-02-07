open! Stdune

type t = (Module.t -> Module.t) Dune_file.Per_module.t

let make preprocess =
  Dune_file.Per_module.map preprocess ~f:(function
    | Dune_file.Preprocess.No_preprocessing -> Module.ml_source
    | Action (_, _) ->
      fun m -> Module.ml_source (Module.pped m)
    | Pps { loc = _; pps = _; flags = _; staged } ->
      if staged then
        Module.ml_source
      else
        fun m -> Module.pped (Module.ml_source m))

let pped_modules (t : t) modules =
  Module.Name.Map.map modules ~f:(fun (m : Module.t) ->
    Dune_file.Per_module.get t (Module.name m) m)
