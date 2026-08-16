open Import

type t =
  | Lib of Lib_name.t
  | Exes of string Nonempty_list.t
  | Melange_entries of string

let equal left right =
  match left, right with
  | Lib left, Lib right -> Lib_name.equal left right
  | Exes (left :: lefts), Exes (right :: rights) ->
    let rec equal_names left right =
      match left, right with
      | [], [] -> true
      | left :: lefts, right :: rights ->
        String.equal left right && equal_names lefts rights
      | [], _ :: _ | _ :: _, [] -> false
    in
    String.equal left right && equal_names lefts rights
  | Melange_entries left, Melange_entries right -> String.equal left right
  | Lib _, (Exes _ | Melange_entries _)
  | Exes _, (Lib _ | Melange_entries _)
  | Melange_entries _, (Lib _ | Exes _) -> false
;;

let for_lib l = Lib l
let for_exes ~names = Exes names
let for_melange ~target = Melange_entries target

(* For debug purposes we use the name of one library or executable and the hash
   of the others if there are multiple executables to name the merlin file *)
let to_string = function
  | Lib name -> sprintf "lib-%s" (Lib_name.to_string name)
  | Exes [ name ] -> sprintf "exe-%s" name
  | Exes (name :: names) ->
    sprintf "exe-%s-%s" name Digest.(repr (Repr.list String.repr) names |> to_string)
  | Melange_entries name -> sprintf "melange-%s" name
;;

let merlin_folder_name = Filename.merlin_conf_dir_basename

let merlin_file_path path ident =
  Filename.concat (Filename.to_string merlin_folder_name) (to_string ident)
  |> Path.Build.relative path
;;
