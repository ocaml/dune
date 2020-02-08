open Stdune
module Re = Dune_re
module Configurator = Configurator.V1

let replace_cwd cwd =
  let abs_path_re = Re.(compile (str cwd)) in
  fun s -> Re.replace_string abs_path_re s ~by:"$TESTCASE_ROOT"

let replace_ext =
  let ext_re =
    Re.(compile (seq [ diff any (char '/'); char '.'; rep1 alnum ]))
  in
  fun tbl s ->
    Re.replace ext_re s ~f:(fun g ->
        let s = Re.Group.get g 0 in
        match List.assoc tbl (String.drop s 1) with
        | None -> s
        | Some res -> sprintf "%c%s" s.[0] res)

let remove_std_arg =
  let spacem, marked_space = Re.mark (Re.rep Re.space) in
  let re =
    Re.(
      compile
        (seq [ marked_space; str "-std="; rep1 (diff any space); marked_space ]))
  in
  fun s ->
    Re.replace re s ~f:(fun g ->
        if Re.Mark.test g spacem then
          " "
        else
          "")

let config_var_replace config : string -> string =
  let vars = [ "ocamlc_cflags"; "ocamlc_cppflags" ] in
  let vars =
    List.filter_map vars ~f:(fun k ->
        Configurator.ocaml_config_var config k
        |> Option.map ~f:(fun v -> (k, v)))
  in
  (* we filter out stuff like -std=gnu99 to get the list of C++ flags *)
  let vars =
    match List.assoc vars "ocamlc_cflags" with
    | None -> vars
    | Some v -> ("cxx_flags", remove_std_arg v) :: vars
  in
  let values =
    List.filter_map vars ~f:(fun (_, v) ->
        match String.trim v with
        | "" -> None
        | v -> Some (Re.str v))
  in
  let re = Re.(compile (alt values)) in
  fun s -> Re.replace_string re s ~by:"$flags"

let make_ext_replace config =
  let tbl =
    let var = Configurator.ocaml_config_var_exn config in
    let exts =
      [ (var "ext_dll", "$ext_dll")
      ; (var "ext_asm", "$ext_asm")
      ; (var "ext_lib", "$ext_lib")
      ; (var "ext_obj", "$ext_obj")
      ]
    in
    (* need to special case exe since we can only remove this extension in
       general *)
    match
      match Configurator.ocaml_config_var config "ext_exe" with
      | Some s -> s
      | None -> (
        match Configurator.ocaml_config_var_exn config "system" with
        | "Win32" -> ".exe"
        | _ -> "" )
    with
    | "" -> exts
    | ext -> (ext, "") :: exts
  in
  List.iter tbl ~f:(fun (e, _) -> assert (e <> ""));
  fun s -> replace_ext tbl s

let () =
  let exit_code = ref 0 in
  let cwd = ref "" in
  let args =
    Arg.align
      [ ( "-exit-code"
        , Arg.Set_int exit_code
        , "NUMBER Exit code of the command for which we are sanitizing the \
           output" )
      ; ( "-cwd"
        , Arg.Set_string cwd
        , "DIR Set the cwd for sanitizing the command output" )
      ]
  in
  let files = ref [] in
  let anon s = files := s :: !files in
  let usage = sprintf "%s [OPTIONS]" (Filename.basename Sys.executable_name) in
  Arg.parse args anon usage;
  let cwd = !cwd
  and exit_code = !exit_code
  and files = List.rev !files in
  let configurator = Configurator.create "sanitizer" in
  let sanitize =
    let ext_replace = make_ext_replace configurator in
    fun s ->
      s |> Ansi_color.strip |> ext_replace |> replace_cwd cwd
      |> config_var_replace configurator
  in
  List.iter files ~f:(fun fn ->
      Io.String_path.read_file fn
      |> sanitize |> String.split_lines
      |> List.iter ~f:(fun line -> Printf.printf "  %s\n" line));
  if exit_code <> 0 then Printf.printf "  [%d]\n" exit_code
