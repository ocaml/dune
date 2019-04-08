let formatter = ref None

let printf fmt =
  match !formatter with
  | None ->
    failwith "Not running an expect test"
  | Some formatter ->
    Format.fprintf formatter fmt
