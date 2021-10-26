open! Stdune
open Import

(* CR-someday amokhov: Most of these records will have [dir = empty]. We might
   want to somehow optimise for the common case, e.g. by switching to a sum type
   with the [Files_only] constructor. It's best not to expose the current
   representation so we can easily change it in future. *)
type t =
  { files : Path.Build.Set.t
  ; dirs : Path.Build.Set.t
  }

module File = struct
  let create file =
    { files = Path.Build.Set.singleton file; dirs = Path.Build.Set.empty }
end

module Files = struct
  let create files = { files; dirs = Path.Build.Set.empty }
end

let create ~files ~dirs = { files; dirs }

let empty = { files = Path.Build.Set.empty; dirs = Path.Build.Set.empty }

let combine x y =
  { files = Path.Build.Set.union x.files y.files
  ; dirs = Path.Build.Set.union x.dirs y.dirs
  }

let is_empty { files; dirs } =
  Path.Build.Set.is_empty files && Path.Build.Set.is_empty dirs

let head { files; dirs } =
  match Path.Build.Set.choose files with
  | Some _ as target -> target
  | None -> Path.Build.Set.choose dirs

let head_exn t =
  match head t with
  | Some target -> target
  | None ->
    Code_error.raise "Targets.head_exn applied to empty set of targets" []

let to_dyn { files; dirs } =
  Dyn.Record
    [ ("files", Path.Build.Set.to_dyn files)
    ; ("dirs", Path.Build.Set.to_dyn dirs)
    ]

let pp { files; dirs } =
  Pp.enumerate
    (Path.Build.Set.to_list files @ Path.Build.Set.to_list dirs)
    ~f:(fun target -> Pp.text (Dpath.describe_target target))

let exists { files; dirs } ~f =
  Path.Build.Set.exists files ~f || Path.Build.Set.exists dirs ~f

let partition_map { files; dirs } ~file ~dir =
  ( Path.Build.Set.to_list_map files ~f:file
  , Path.Build.Set.to_list_map dirs ~f:dir )

let iter { files; dirs } ~file ~dir =
  Path.Build.Set.iter files ~f:file;
  Path.Build.Set.iter dirs ~f:dir

let map { files; dirs } ~f = f ~files ~dirs

let fold { files; dirs } ~init ~file ~dir =
  let init = Path.Build.Set.fold files ~init ~f:file in
  Path.Build.Set.fold dirs ~init ~f:dir

module Validation_result = struct
  type t =
    | Valid of { parent_dir : Path.Build.t }
    | No_targets
    | Inconsistent_parent_dir
    | File_and_directory_target_with_the_same_name of Path.Build.t
end

let validate t =
  match is_empty t with
  | true -> Validation_result.No_targets
  | false -> (
    match Path.Build.Set.inter t.files t.dirs |> Path.Build.Set.choose with
    | Some path -> File_and_directory_target_with_the_same_name path
    | None -> (
      let parent_dir = Path.Build.parent_exn (head_exn t) in
      match
        exists t ~f:(fun path -> Path.Build.(parent_exn path <> parent_dir))
      with
      | true -> Inconsistent_parent_dir
      | false -> Valid { parent_dir }))
