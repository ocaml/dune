open Import
open Common

let calculate_column_widths = function
  | [] -> []
  | first_row :: _ as rows ->
    List.init (List.length first_row) ~f:(fun i ->
      List.fold_left rows ~init:0 ~f:(fun acc row ->
        List.nth row i |> Option.value ~default:"" |> String.length |> max acc))
;;

let print_table_row ?(sep = " | ") widths values =
  List.map2 widths values ~f:(Printf.sprintf "%-*s")
  |> String.concat ~sep
  |> Console.printf_always "%s"
;;

let print_table_separator widths =
  List.map widths ~f:(fun len -> String.make len '-') |> print_table_row ~sep:"-|-" widths
;;

let print_table = function
  | [] -> Code_error.raise "empty table" []
  | header :: data_rows as rows ->
    let widths = calculate_column_widths rows in
    print_table_row widths header;
    print_table_separator widths;
    List.iter data_rows ~f:(print_table_row widths)
;;

let head_commit lines =
  (* The first line of git ls-remote output is always HEAD *)
  List.find_map lines ~f:(fun line ->
    match String.split ~on:'\t' line with
    | [ commit; "HEAD" ] -> Some commit
    | _ -> None)
;;

let find_tag_commit tag ~url =
  let prefix = "refs/tags/" in
  Git.ls_remote_tags ~url
  >>| List.find_map ~f:(fun line ->
    match String.split ~on:'\t' line with
    | [ commit; ref ] when String.is_prefix ref ~prefix ->
      let tag_name = String.drop_prefix_if_exists ref ~prefix in
      if String.equal tag_name tag && not (String.is_suffix tag_name ~suffix:"^{}")
      then Some commit
      else None
    | _ -> None)
;;

let check_package_updates package_name package_specs =
  Fiber.parallel_map
    package_specs
    ~f:(fun { Packages.source = Git { url; revision; _ }; _ } ->
      Console.print_status "Checking %s (%s)" package_name url ~f:(fun () ->
        let* lines = Git.ls_remote ~url in
        let format_package_name tag_opt =
          let repo_name =
            Filename.basename url |> String.drop_suffix_if_exists ~suffix:".git"
          in
          match Int.compare (List.length package_specs) 1, tag_opt with
          | Gt, Some tag -> Printf.sprintf "%s (%s) (%s)" package_name repo_name tag
          | Gt, None -> Printf.sprintf "%s (%s)" package_name repo_name
          | (Lt | Eq), Some tag -> Printf.sprintf "%s (%s)" package_name tag
          | (Lt | Eq), None -> package_name
        in
        match revision, head_commit lines with
        | Tag current_tag, Some head ->
          find_tag_commit current_tag ~url
          >>| (function
           | Some tag_commit_hash ->
             ( format_package_name (Some current_tag)
             , tag_commit_hash
             , if String.equal tag_commit_hash head then `Up_to_date else `Outdated head )
           | None -> format_package_name None, "could not find tag " ^ current_tag, `Error)
        | Commit current_commit, Some head ->
          Fiber.return
            ( format_package_name None
            , current_commit
            , if String.equal current_commit head then `Up_to_date else `Outdated head )
        | _ ->
          Fiber.return (format_package_name None, "could not determine status", `Error)))
;;

let outdated_cmd options =
  run_with_scheduler options
  @@ fun () ->
  let+ data =
    let module String_map_parallel = Fiber.Make_parallel_map (String.Map) in
    get_packages_to_process options
    |> List.fold_left ~init:String.Map.empty ~f:(fun acc spec ->
      String.Map.add_multi acc spec.Packages.name spec)
    |> String_map_parallel.parallel_map ~f:check_package_updates
  in
  String.Map.values data
  |> List.concat
  |> List.filter_map ~f:(fun (name, current, status) ->
    Option.some_if
      (!Console.verbose || status <> `Up_to_date)
      [ name
      ; current
      ; (match status with
         | `Up_to_date -> "up to date"
         | `Outdated commit -> commit
         | `Error -> "error")
      ])
  |> List.cons [ "Package"; "Current"; "HEAD (upstream)" ]
  |> print_table
;;

let list_cmd () =
  List.map
    Packages.all_packages
    ~f:(fun { Packages.name; source = Git { url; revision; _ }; _ } ->
      [ name
      ; (match revision with
         | Commit commit -> String.take commit 8
         | Tag tag -> tag)
      ; url
      ])
  |> List.cons [ "Name"; "Revision"; "URL" ]
  |> print_table
;;

let outdated_command =
  let open Cmdliner in
  Cmd.v (Cmd.info "outdated" ~doc:"Check for outdated packages")
  @@ let+ options = package_term in
     outdated_cmd options
;;

let list_command =
  let open Cmdliner in
  Cmd.v (Cmd.info "list" ~doc:"List available packages") Term.(const list_cmd $ const ())
;;
