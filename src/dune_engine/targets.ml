open! Stdune
open Import

type t = { files : Path.Build.Set.t } [@@unboxed]

module File = struct
  let create file = { files = Path.Build.Set.singleton file }
end

module Files = struct
  let create files = { files }
end

let empty = { files = Path.Build.Set.empty }

let combine x y = { files = Path.Build.Set.union x.files y.files }

let is_empty t = Path.Build.Set.is_empty t.files

let head { files } = Path.Build.Set.choose files

let head_exn t =
  match head t with
  | Some target -> target
  | None ->
    Code_error.raise "Targets.head_exn applied to empty set of targets" []

let to_dyn { files } = Dyn.Record [ ("files", Path.Build.Set.to_dyn files) ]

let pp { files } =
  Pp.enumerate (Path.Build.Set.to_list files) ~f:(fun target ->
      Pp.text (Dpath.describe_target target))

let exists { files } ~file = Path.Build.Set.exists files ~f:file

let to_list_map { files } ~file = Path.Build.Set.to_list_map files ~f:file

let fold { files } ~init ~file = Path.Build.Set.fold files ~init ~f:file

let iter { files } ~file = Path.Build.Set.iter files ~f:file

let files t = t.files

module Validation_result = struct
  type t =
    | Valid of { parent_dir : Path.Build.t }
    | No_targets
    | Inconsistent_parent_dir
end

let validate t =
  match is_empty t with
  | true -> Validation_result.No_targets
  | false -> (
    let parent_dir = Path.Build.parent_exn (head_exn t) in
    match
      exists t ~file:(fun target ->
          Path.Build.(parent_exn target <> parent_dir))
    with
    | true -> Inconsistent_parent_dir
    | false -> Valid { parent_dir })
