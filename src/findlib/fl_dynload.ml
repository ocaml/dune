(* $Id$ *)

(* Utilities for loading dynamically packages *)

let load_pkg pkg =
  if not (Findlib.is_recorded_package pkg) then (
     (* Determine the package directory: *)
     let d = Findlib.package_directory pkg in
     (* First try the new "plugin" variable: *)
     let preds = Findlib.recorded_predicates() in
     let archive =
       try
         Findlib.package_property preds pkg "plugin"
       with
         | Not_found ->
              (* Legacy: use "archive" but require that the predicate
                 "plugin" is mentioned in the definition
               *)
              try
                let v, fpreds =
                  Findlib.package_property_2 ("plugin"::preds) pkg "archive" in
                let need_plugin =
                  List.mem "native" preds in
                if need_plugin && not (List.mem (`Pred "plugin") fpreds) then
                  ""
                else
                  v
              with Not_found -> "" in
     (* Split the plugin/archive property and resolve the files: *)
     let files = Fl_split.in_words archive in
     List.iter 
       (fun file ->
          let file = Findlib.resolve_path ~base:d file in
          Dynlink.loadfile file
       ) files;
     Findlib.record_package Findlib.Record_load pkg
  )


let load_packages pkgs =
  let preds = Findlib.recorded_predicates() in
  let eff_pkglist =
    Findlib.package_deep_ancestors preds pkgs in
  List.iter load_pkg eff_pkglist
