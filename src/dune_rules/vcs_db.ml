open Import
open Memo.O

let ancestor_vcs =
  Memo.lazy_ ~name:"ancestor_vcs" (fun () ->
    if Execution_env.inside_dune
    then Memo.return None
    else (
      let rec loop dir =
        if Fpath.is_root dir
        then None
        else (
          let dir = Filename.dirname dir in
          match
            Sys.readdir dir
            |> Array.to_list
            |> Filename.Set.of_list
            |> Vcs.Kind.of_dir_contents
          with
          | Some kind -> Some { Vcs.kind; root = Path.of_string dir }
          | None -> loop dir)
      in
      Memo.return (loop (Path.to_absolute_filename Path.root))))
;;

let fold_parents =
  let rec loop acc f t = function
    | [] -> Memo.return acc
    | comp :: components ->
      (match Filename.Map.find (Source_tree.Dir.sub_dirs t) comp with
       | None -> Memo.return acc
       | Some sub_dir ->
         let* sub_dir = Source_tree.Dir.sub_dir_as_t sub_dir in
         let* acc = f sub_dir acc in
         loop acc f sub_dir components)
  in
  fun path ~init ~f ->
    let components = Path.Source.explode path in
    let* root = Source_tree.root () in
    let* acc = f root init in
    loop acc f root components
;;

(* there's no need for any memoization. we use this function sporadically and
   it's already fast enough *)
let nearest_vcs =
  let f dir acc =
    Readdir.of_source_path (Source_tree.Dir.path dir)
    >>| function
    | Error _ -> acc
    | Ok readdir ->
      (match
         Readdir.dirs readdir |> List.find_map ~f:(fun (s, _) -> Vcs.Kind.of_dir_name s)
       with
       | None -> acc
       | Some kind -> Some { Vcs.kind; root = Path.source @@ Source_tree.Dir.path dir })
  in
  fun path ->
    let open Memo.O in
    let* init = Memo.Lazy.force ancestor_vcs in
    fold_parents ~f ~init path
;;
