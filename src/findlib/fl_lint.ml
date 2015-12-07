(* $Id$ -*- tuareg -*-
 * ----------------------------------------------------------------------
 *
 *)

open Fl_metascanner

module Have = struct
  module T = struct
    type t = [
        `BothByteNative      (** archive(native,byte), plugin(native,byte) *)
      | `NoByteNative        (** archive(...), plugin(...) no native nor byte *)
      | `Archive of [`Plugin|`NoPlugin] * [`Byte | `Native]
        (** archive(plugin,...) or archive(...)) *)
      | `Plugin of [`Plugin|`NoPlugin] * [`Byte | `Native]
        (** plugin(...) *)
    ]
    let compare = compare
  end
  include T
  module Set = Set.Make(T)
  module Map = Map.Make(T)
end

let scan_def acc def =
  let add have = Have.Map.add have def acc in
  let has_plugin_pred = List.mem (`Pred "plugin") def.def_preds in
  let plugin = if has_plugin_pred then `Plugin else `NoPlugin in
  let has_byte_pred = List.mem (`Pred "byte") def.def_preds in
  let has_native_pred = List.mem (`Pred "native") def.def_preds in
  (** mode is used only when there is exactly one of "byte","native" *)
  let mode = if has_byte_pred then `Byte else `Native in
  match def.def_var with
  | ("archive"|"plugin") when not (has_byte_pred || has_native_pred) ->
    add `NoByteNative
  | ("archive"|"plugin") when has_byte_pred && has_native_pred ->
    add `BothByteNative
  | "archive" -> add (`Archive(plugin,mode))
  | "plugin"  -> add (`Plugin(plugin,mode))
  | _ -> acc


let warn_def ~warned pkg =
  let haves =
    List.fold_left scan_def Have.Map.empty pkg.pkg_defs
  in
  let mem x  = Have.Map.mem x haves in
  let find x = Have.Map.find x haves in
  let warning fmt = warned := true; Printf.printf fmt in
  let if_ ~has ?has_not msg =
    if mem has && (match has_not with None -> true | Some h -> not (mem h))
    then warning msg print_def (find has);
  in
  if_ ~has:`BothByteNative
    "%aThis variable shouldn't have at the same time \
     \"byte\" and \"native\".\n\n";
  if_ ~has:`NoByteNative
    "%aThis variable should have at least the predicate \
     \"byte\" or \"native\".\n\n";
  let with_mode mode =
    if_ ~has:(`Plugin (`Plugin,mode))
      "%aYou must not add the predicate \"plugin\" to the variable \
       \"plugin\".\n\n";
    if_ ~has:(`Archive (`Plugin,mode)) ~has_not:(`Plugin (`NoPlugin,mode))
      "%aThis specification of dynamic loading is deprecated, you should add a \
       \"plugin(...)\" variable.\n\n";
    if_ ~has:(`Archive (`NoPlugin,mode)) ~has_not:(`Plugin (`NoPlugin,mode))
      "%aThis variable indicates how to link statically, you should add a \
       \"plugin(...)\" variable for linking dynamically.\n\n";
  in
  with_mode `Byte;
  with_mode `Native

let warn pkg =
  let warned = ref false in
  let rec aux pkg =
    warn_def ~warned pkg;
    List.iter (fun (_,pkg) -> aux pkg) pkg.pkg_children;
  in
  aux pkg;
  !warned
