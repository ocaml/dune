open Stdune
module Re = Dune_re
module Dune_cram = Dune_cramlib.V1
module Configurator = Configurator.V1

let ext_replace =
  let tbl =
    lazy
      (let config = Configurator.create "sanitizer" in
       List.filter_map [ "ext_exe"; "ext_dll"; "ext_asm"; "ext_lib"; "ext_obj" ]
         ~f:(fun var ->
           match Configurator.ocaml_config_var config var with
           | Some "" -> None
           | Some s -> Some (s, "$" ^ var)
           | None -> (
             match (var, Configurator.ocaml_config_var config "system") with
             | "ext_exe", Some "Win32" -> Some (".exe", var)
             | _ -> None )))
  in
  let re =
    lazy
      Re.(
        compile
          (seq
             [ diff any (char '/')
             ; alt (List.map (Lazy.force tbl) ~f:(fun (s, _) -> str s))
             ; eow
             ]))
  in
  let map =
    lazy (String.Map.of_list_reduce (Lazy.force tbl) ~f:(fun _ x -> x))
  in
  Dune_cram.Sanitizer.make (fun s ->
      Re.replace (Lazy.force re) s ~f:(fun g ->
          let s = Re.Group.get g 0 in
          sprintf "%c%s" s.[0]
            (String.Map.find_exn (Lazy.force map) (String.drop s 1))))

let cram =
  let default_sanitizer =
    let open Dune_cram.Sanitizer.O in
    Dune_cram.Sanitizer.rewrite_build_path_prefix_map >>> ext_replace
  in
  Dune_cram.make ~default_sanitizer ()

let () = Dune_cram.run cram
