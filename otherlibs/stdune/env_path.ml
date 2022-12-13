let var = "PATH"

let cons env ~dir =
  Env.update env ~var ~f:(fun _PATH -> Some (Bin.cons_path dir ~_PATH))

let path env =
  match Env.get env var with
  | None -> []
  | Some s -> Bin.parse_path s
