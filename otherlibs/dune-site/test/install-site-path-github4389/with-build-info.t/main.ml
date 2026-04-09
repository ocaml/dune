let version =
  match Build_info.V1.version () with
  | None   -> "n/a"
  | Some v -> Build_info.V1.Version.to_string v

let () =
  Format.eprintf "%s@." version;
  List.iter
    (fun x -> Format.eprintf "%s@." x)
    SitesModule.Sites.github4389
