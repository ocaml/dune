
open! Stdune

type 'set t =
  { sub_dirs : 'set
  ; data_only : 'set
  }

let default =
  let standard_dirs =
    Predicate_lang.of_pred (function
      | "" -> false
      | s -> s.[0] <> '.' && s.[0] <> '_')
  in
  { sub_dirs = standard_dirs
  ; data_only = Predicate_lang.empty
  }

let make ~sub_dirs ~ignored_sub_dirs ~data_only =
  let sub_dirs =
    let ignored_sub_dirs = Predicate_lang.union ignored_sub_dirs in
    let sub_dirs = Option.value sub_dirs ~default:default.sub_dirs in
    Predicate_lang.diff sub_dirs ignored_sub_dirs
  in
  let data_only = Option.value data_only ~default:default.data_only in
  { sub_dirs ; data_only }

let ignore_dirs t ~dirs =
  { t with sub_dirs = Predicate_lang.diff t.sub_dirs dirs }

let eval t ~dirs =
  let sub_dirs =
    Predicate_lang.filter t.sub_dirs ~standard:default.sub_dirs dirs
  in
  let data_only =
    Predicate_lang.filter t.data_only ~standard:default.data_only sub_dirs
    |> String.Set.of_list
  in
  let sub_dirs = String.Set.of_list sub_dirs in
  { sub_dirs
  ; data_only
  }

module Status = struct
  type t = Ignored | Data_only | Normal
end

let status t ~dir =
  match String.Set.mem t.sub_dirs dir
      , String.Set.mem t.data_only dir
  with
  | true, false  -> Status.Normal
  | true, true   -> Data_only
  | false, false -> Ignored
  | false, true  -> assert false
