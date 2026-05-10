type t =
  { dir : Path.t
  ; filenames : Filename.Array.Set.t
  }

let equal t { dir; filenames } =
  Path.equal t.dir dir && Filename.Array.Set.equal filenames t.filenames
;;

let dir { dir; filenames = _ } = dir
let filenames { dir = _; filenames } = filenames
let empty ~dir = { dir; filenames = Filename.Array.Set.empty }
let is_empty { dir = _; filenames } = Filename.Array.Set.is_empty filenames

let create ?filter ~dir filenames =
  match filter with
  | None -> { dir; filenames }
  | Some f ->
    { dir
    ; filenames = Filename.Array.Set.filter filenames ~f:(fun basename -> f ~basename)
    }
;;

let to_list { dir; filenames } =
  Filename.Array.Set.to_list_map filenames ~f:(Path.relative dir)
;;
