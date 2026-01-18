open Import
open Memo.O

type t = Package.Name.Set.t Path.Source.Map.t

let packages_under_dir by_dir ~dir =
  Path.Source.Map.foldi by_dir ~init:Package.Name.Set.empty ~f:(fun pkg_dir names acc ->
    if Path.Source.is_descendant pkg_dir ~of_:dir
    then Package.Name.Set.union acc names
    else acc)
  |> Package.Name.Set.to_list
;;

let scope_db =
  Memo.lazy_ ~name:"package-scope-db" (fun () ->
    let* all_packages = Dune_load.packages ()
    and* scope_stanzas = Dune_load.scopes () in
    let packages_by_dir =
      Package.Name.Map.fold all_packages ~init:Path.Source.Map.empty ~f:(fun pkgs acc ->
        List.fold_left pkgs ~init:acc ~f:(fun acc (pkg, _status) ->
          let dir = Package.dir pkg in
          Path.Source.Map.update acc dir ~f:(function
            | None -> Some (Package.Name.Set.singleton (Package.name pkg))
            | Some set -> Some (Package.Name.Set.add set (Package.name pkg)))))
    in
    Memo.return
      (Path.Source.Map.mapi scope_stanzas ~f:(fun scoped_dir stanza ->
         let standard = packages_under_dir packages_by_dir ~dir:scoped_dir in
         Dune_lang.Scope_stanza.eval_packages stanza ~standard)))
;;

let find_scope_for_dir scopes src_dir =
  let rec find_in_ancestors dir =
    match Path.Source.Map.find scopes dir with
    | Some allowed -> Some (dir, allowed)
    | None ->
      (match Path.Source.parent dir with
       | None -> None
       | Some parent -> find_in_ancestors parent)
  in
  find_in_ancestors src_dir
;;

let packages_and_mask =
  Memo.lazy_ ~name:"package-scope-packages" (fun () ->
    let+ scopes = Memo.Lazy.force scope_db
    and+ all_packages = Dune_load.packages () in
    let packages =
      Package.Name.Map.fold all_packages ~init:Package.Name.Map.empty ~f:(fun pkgs acc ->
        let visible_pkgs =
          List.filter pkgs ~f:(fun (pkg, _status) ->
            let pkg_dir = Package.dir pkg in
            match find_scope_for_dir scopes pkg_dir with
            | None -> true
            | Some (_, allowed) -> Package.Name.Set.mem allowed (Package.name pkg))
        in
        match visible_pkgs with
        | [] -> acc
        | [ (pkg, status) ] ->
          Package.Name.Map.add_exn acc (Package.name pkg) (pkg, status)
        | (pkg1, _) :: (pkg2, _) :: _ ->
          User_error.raise
            [ Pp.textf
                "The package %S is defined more than once:"
                (Package.Name.to_string (Package.name pkg1))
            ; Pp.textf "- %s" (Loc.to_file_colon_line (Package.loc pkg1))
            ; Pp.textf "- %s" (Loc.to_file_colon_line (Package.loc pkg2))
            ])
    in
    let mask = Only_packages.mask (Package.Name.Map.map packages ~f:(fun x -> [ x ])) in
    let packages =
      Only_packages.filter_packages mask (Package.Name.Map.map packages ~f:fst)
    in
    packages, mask)
;;

let packages () =
  let+ packages, _ = Memo.Lazy.force packages_and_mask in
  packages
;;

let mask () =
  let+ _, mask = Memo.Lazy.force packages_and_mask in
  mask
;;

let db () = Memo.Lazy.force scope_db
