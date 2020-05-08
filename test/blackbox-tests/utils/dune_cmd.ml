open Stdune

exception Invalid_command_line

let commands = Table.create (module String) 10

let register name of_args run =
  Table.add_exn commands name (fun args ->
      let t = of_args args in
      run t)

module Stat = struct
  type data =
    | Hardlinks
    | Permissions

  type t =
    { file : Path.t
    ; data : data
    }

  let data_of_string = function
    | "hardlinks" -> Hardlinks
    | "permissions" -> Permissions
    | s -> failwith ("invalid data " ^ s)

  let pp_stats data (stats : Unix.stats) =
    match data with
    | Hardlinks -> Int.to_string stats.st_nlink
    | Permissions -> sprintf "%o" stats.st_perm

  let name = "stat"

  let of_args = function
    | [ data; file ] ->
      let data = data_of_string data in
      let file = Path.of_filename_relative_to_initial_cwd file in
      { file; data }
    | _ -> raise Invalid_command_line

  let run { file; data } =
    let stats = Path.stat file in
    print_endline (pp_stats data stats)

  let () = register name of_args run
end

module Cat = struct
  type t = File of Path.t

  let name = "cat"

  let of_args = function
    | [ file ] -> File (Path.of_filename_relative_to_initial_cwd file)
    | _ -> raise Invalid_command_line

  let run (File p) = print_string (Io.read_file p)

  let () = register name of_args run
end

let () =
  let name, args =
    match Array.to_list Sys.argv with
    | _ :: name :: args -> (name, args)
    | [] -> assert false
    | [ _ ] ->
      Format.eprintf "No arguments passed@.%!";
      exit 1
  in
  match Table.find commands name with
  | None ->
    Format.eprintf "No command %S name found" name;
    exit 1
  | Some run -> run args
