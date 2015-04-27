(* $Id$ *)

(* Utilities for loading dynamically packages *)

let load_pkg pkg =
  if not (Findlib.is_recorded_package pkg) then (
     (* Determine the package directory: *)
     let d = Findlib.package_directory pkg in
     (* Determine the 'archive(plugin,...)' property: *)
     let preds = "plugin" :: Findlib.recorded_predicates() in
     let archive =
       try
         Findlib.package_property preds pkg "archive"
       with Not_found -> "" in
     (* Split the 'archive' property and resolve the files: *)
     let files = Fl_split.in_words archive in
     List.iter 
       (fun file ->
          let file = Findlib.resolve_path ~base:d file in
          Dynlink.loadfile file
       ) files;
     Findlib.record_package Findlib.Record_load pkg
  )


let load_packages pkgs =
  let preds = "plugin" :: Findlib.recorded_predicates() in
  let eff_pkglist =
    Findlib.package_deep_ancestors preds pkgs in
  List.iter load_pkg eff_pkglist
