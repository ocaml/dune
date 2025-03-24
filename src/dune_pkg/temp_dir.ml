open Stdune

let in_build =
  lazy
    (let dir = Path.relative (Path.build Path.Build.root) ".temp" in
     Path.mkdir_p dir;
     dir)
;;

let dir_for_target ~target ~prefix ~suffix =
  match (target : Path.t) with
  | In_build_dir _ -> Temp.temp_in_dir Dir ~dir:(Lazy.force in_build) ~prefix ~suffix
  | _ ->
    let parent = Path.parent_exn target in
    Path.mkdir_p parent;
    Temp.temp_in_dir Dir ~dir:parent ~prefix ~suffix
;;
