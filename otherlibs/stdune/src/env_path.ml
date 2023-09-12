let var = "PATH"

let cons ?(var = var) env ~dir =
  Env.update env ~var ~f:(fun _PATH -> Some (Bin.cons_path dir ~_PATH))
;;

(* [cons_multi env ~dirs] adds each path in [dirs] to the start of the PATH
   variable in [env], preserving their order *)
let cons_multi env ~dirs =
  Env.update env ~var ~f:(fun init ->
    List.fold_right dirs ~init ~f:(fun dir acc -> Some (Bin.cons_path dir ~_PATH:acc)))
;;

let path env =
  match Env.get env var with
  | None -> []
  | Some s -> Bin.parse_path s
;;

let extend_env_concat_path a b =
  let a_including_b's_path = cons_multi a ~dirs:(path b) in
  let b_without_path = Env.remove b ~var in
  Env.extend_env a_including_b's_path b_without_path
;;
