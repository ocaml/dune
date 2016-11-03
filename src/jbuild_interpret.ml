open Import
open Sexp.Of_sexp

module Lib = struct
  type t =
    { name        : string
    ; public_name : string option
    ; libraries   : string list
    ; modules     : String_set.t
    ; c_flags     : string list
    ; c_names     : string list
    }

  let guess_modules ~dir ~files_produced_by_rules =
    Sys.readdir dir
    |> Array.to_list
    |> List.append files_produced_by_rules
    |> List.filter ~f:(fun fn ->
        Filename.check_suffix fn ".mli"
        || Filename.check_suffix fn ".ml")
    |> List.map ~f:(fun fn ->
        String.capitalize (Filename.chop_extension fn))
    |> String_set.of_list

  let parse ~dir ~files_produced_by_rules sexp =
    record
      [ field   "name"        string
      ; field_o "public_name" string
      ; field   "libraries"   (list string) ~default:[]
      ; field_o "modules"     string_set
      ; field   "c_flags"     (list string) ~default:[]
      ; field   "c_names"     (list string) ~default:[]
      ]
      (fun name public_name libraries modules c_flags c_names ->
         let modules =
           match modules with
           | None ->
             guess_modules ~dir ~files_produced_by_rules
           | Some x -> x
         in
         { name
         ; public_name
         ; libraries
         ; modules
         ; c_flags
         ; c_names
         })
      sexp

(*  let setup_rules ~dir t =
    let pped_files =
      List.map t.modules ~f:(fun m ->
        dir ^/ String.uncapitalize m ^ ".pp")
    in
    let depends_fn = dir ^/ ".depends" in
    rule ~deps:(Files pped_files) ~targets:(Files [depends_fn]) (fun () ->
         run ~stdout_to:depends_fn "ocamldep" pped_files);
    rule ~deps:(Files [depends_fn]) ~targets:(Vals [source_deps]) (fun () ->
        (* parse *)
        return [deps]);
    List.iter t.modules ~f:(fun m ->
        let src = dir ^/ String.uncapitalize m ^ ".ml" in
        let dst = dir ^/ t.name ^ "__" ^ m ^ ".cmo" in
        rule ~deps:(Both (src, [source_deps])) ~targets:(Files [dst])
          (fun deps ->
             List.iter (String_map.find deps m) ~f:(fun m -> wait_for_file (... ^ m ^ ".cmi")) >>= fun () ->
    run "ocamlc" ["-c"; src]);*)
end

module Rule = struct
  type t =
    { targets : string list
    ; deps    : string list
    ; action  : string
    }

  let parse sexp =
    let open Sexp.Of_sexp in
    record
      [ field "targets" (list string)
      ; field "deps"    (list string)
      ; field "action"  string
      ]
      (fun targets deps action ->
         { targets; deps; action })
      sexp
end

module Jbuild = struct
  type t =
    | Library of Lib.t
    | Rule    of Rule.t

  let parse ~dir (sexps : Sexp.t list) =
    let rules =
      List.filter_map sexps ~f:(function
        | List [Atom "rule"; arg] ->
          Some (Rule.parse arg)
        | _ -> None)
    in
    let files_produced_by_rules =
      List.concat_map rules ~f:(fun r -> r.targets)
    in
    let libs =
      List.filter_map sexps ~f:(function
        | List [Atom "library"; arg] ->
          Some (Library (Lib.parse ~dir ~files_produced_by_rules arg))
        | _ ->
          None)
    in
    List.map rules ~f:(fun r -> Rule r) @ libs

  let load ~dir =
    let fn = dir ^/ "jbuild" in
    let ic = open_in fn in
    let sexps = Sexp_lexer.many (Lexing.from_channel ic) |> List.map ~f:fst in
    close_in ic;
    parse ~dir sexps
end

let load_conf () =
  let rec walk dir acc =
    let files = Sys.readdir dir |> Array.to_list |> String_set.of_list in
    let ignore =
      if String_set.mem "jbuild-ignore" files then
        lines_of_file (dir ^/ "jbuild-ignore") |> String_set.of_list
      else
        String_set.empty
    in
    let acc =
      String_set.fold files ~init:acc ~f:(fun fn acc ->
        if String_set.mem fn ignore then
          acc
        else
          let fn = dir ^/ fn in
          if Sys.is_directory fn then
            walk fn acc
          else
            acc)
    in
    if String_set.mem "jbuild" files then
      Jbuild.load ~dir @ acc
    else
      acc
  in
  walk Filename.current_dir_name []
