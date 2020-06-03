let custom = (match Build_info.V2.custom () with
  | None -> "n/a"
  | Some v -> Build_info.V2.Custom.to_string v)
