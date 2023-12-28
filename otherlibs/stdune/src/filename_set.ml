(* CR-someday amokhov: Switch from sets to "flat sets" backed by immutable arrays. *)
type t =
  { dir : Path.t
  ; filenames : Filename.Set.t
  }

let equal t { dir; filenames } =
  Path.equal t.dir dir && Filename.Set.equal filenames t.filenames
;;

let dir { dir; filenames = _ } = dir
let filenames { dir = _; filenames } = filenames
let empty ~dir = { dir; filenames = String.Set.empty }
let is_empty { dir = _; filenames } = Filename.Set.is_empty filenames

let create ?filter ~dir filenames =
  match filter with
  | None -> { dir; filenames }
  | Some f ->
    { dir
    ; filenames =
        Filename.Set.to_list filenames
        |> List.filter ~f:(fun basename -> f ~basename)
        |> Filename.Set.of_list
    }
;;

let to_list { dir; filenames } = Filename.Set.to_list_map filenames ~f:(Path.relative dir)
