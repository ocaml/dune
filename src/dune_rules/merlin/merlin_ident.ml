open Import

type t =
  | Lib of Lib_name.t
  | Exe_target of Exe_target.t

let for_lib l = Lib l
let for_exe_target target = Exe_target target

(* For debug purposes we use the name of one library or executable and the hash
   of the others if there are multiple executables to name the merlin file *)
let to_string = function
  | Lib name -> sprintf "lib-%s" (Lib_name.to_string name)
  | Exe_target target ->
    (match Exe_target.compilation_mode target with
     | Melange -> sprintf "melange-%s" (Exe_target.first_name target)
     | Ocaml ->
       (match Exe_target.names target with
        | [ name ] -> sprintf "exe-%s" name
        | name :: names ->
          sprintf
            "exe-%s-%s"
            name
            Digest.(repr (Repr.list String.repr) names |> to_string)))
;;

let merlin_folder_name = Filename.merlin_conf_dir_basename

let merlin_file_path path ident =
  Path.Build.relative
    (Path.Build.relative_fname path merlin_folder_name)
    (to_string ident)
;;
