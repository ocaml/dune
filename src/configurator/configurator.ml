open Base
open Stdio

module Sys        = Caml.Sys
module Fn         = Caml.Filename
module Arg        = Caml.Arg
module Buffer     = Caml.Buffer
module Pervasives = Caml.Pervasives

let ( ^^ ) = Caml.( ^^ )
let ( ^/ ) = Fn.concat
let sprintf = Printf.sprintf

exception Fatal_error of string

let die fmt =
  Printf.ksprintf (fun s ->
    raise (Fatal_error s);
  ) fmt

type t =
  { name              : string
  ; dest_dir          : string
  ; ocamlc            : string
  ; log               : string -> unit
  ; mutable counter   : int
  ; ext_obj           : string
  ; c_compiler        : string
  ; stdlib_dir        : string
  ; ccomp_type        : string
  ; ocamlc_config     : string Map.M(String).t
  ; ocamlc_config_cmd : string
  }

let rec rm_rf dir =
  Array.iter (Sys.readdir dir) ~f:(fun fn ->
    let fn = dir ^/ fn in
    if Sys.is_directory fn then
      rm_rf fn
    else
      Unix.unlink fn);
  Unix.rmdir dir

module Temp = struct
  (* Copied from filename.ml and adapted for directories *)

  let prng = lazy(Random.State.make_self_init ())

  let gen_name ~temp_dir ~prefix ~suffix =
    let rnd = Int.bit_and (Random.State.bits (Lazy.force prng)) 0xFFFFFF in
    temp_dir ^/ (Printf.sprintf "%s%06x%s" prefix rnd suffix)

  let create ~prefix ~suffix ~mk =
    let temp_dir = Fn.get_temp_dir_name () in
    let rec try_name counter =
      let name = gen_name ~temp_dir ~prefix ~suffix in
      match mk name with
      | () -> name
      | exception (Unix.Unix_error _) when counter < 1000 ->
        try_name (counter + 1)
    in
    try_name 0

  let create_temp_dir ~prefix ~suffix =
    let dir = create ~prefix ~suffix ~mk:(fun name -> Unix.mkdir name 0o700) in
    Caml.at_exit (fun () -> rm_rf dir);
    dir
end

module Find_in_path = struct
  let path_sep =
    if Sys.win32 then
      ';'
    else
      ':'

  let get_path () =
    match Sys.getenv "PATH" with
    | exception Not_found -> []
    | s -> String.split s ~on:path_sep

  let exe = if Sys.win32 then ".exe" else ""

  let prog_not_found prog =
    die "Program %s not found in PATH" prog

  let best_prog dir prog =
    let fn = dir ^/ prog ^ ".opt" ^ exe in
    if Sys.file_exists fn then
      Some fn
    else
      let fn = dir ^/ prog ^ exe in
      if Sys.file_exists fn then
        Some fn
      else
        None

  let find_ocaml_prog prog =
    match
      List.find_map (get_path ()) ~f:(fun dir ->
        best_prog dir prog)
    with
    | None -> prog_not_found prog
    | Some fn -> fn

  let find prog =
    List.find_map (get_path ()) ~f:(fun dir ->
      let fn = dir ^/ prog ^ exe in
      Option.some_if (Sys.file_exists fn) fn)
end

let logf t fmt = Printf.ksprintf t.log fmt

let gen_id t =
  let n = t.counter in
  t.counter <- n + 1;
  n

type run_result =
  { exit_code : int
  ; stdout    : string
  ; stderr    : string
  }

let quote =
  let need_quote = function
    | ' ' | '\"' -> true
    | _          -> false
  in
  fun s ->
    if String.is_empty s || String.exists ~f:need_quote s
    then Fn.quote s
    else s

let command_line prog args =
  String.concat ~sep:" " (List.map (prog :: args) ~f:quote)

let run t ~dir cmd =
  logf t "run: %s" cmd;
  let n = gen_id t in
  let stdout_fn = t.dest_dir ^/ sprintf "stdout-%d" n in
  let stderr_fn = t.dest_dir ^/ sprintf "stderr-%d" n in
  let exit_code =
    Printf.ksprintf
      Sys.command "cd %s && %s > %s 2> %s"
      (Fn.quote dir)
      cmd
      (Fn.quote stdout_fn)
      (Fn.quote stderr_fn)
  in
  let stdout = In_channel.read_all stdout_fn in
  let stderr = In_channel.read_all stderr_fn in
  logf t "-> process exited with code %d" exit_code;
  logf t "-> stdout:";
  List.iter (String.split_lines stdout) ~f:(logf t " | %s");
  logf t "-> stderr:";
  List.iter (String.split_lines stderr) ~f:(logf t " | %s");
  { exit_code; stdout; stderr }

let run_capture_exn t ~dir cmd =
  let { exit_code; stdout; stderr } = run t ~dir cmd in
  if exit_code <> 0 then
    die "command exited with code %d: %s" exit_code cmd
  else if not (String.is_empty stderr) then
    die "command has non-empty stderr: %s" cmd
  else
    stdout

let run_ok t ~dir cmd = (run t ~dir cmd).exit_code = 0

let get_ocaml_config_var_exn ~ocamlc_config_cmd map var =
  match Map.find map var with
  | None -> die "variable %S not found in the output of `%s`" var ocamlc_config_cmd
  | Some s -> s

let ocaml_config_var t var = Map.find t.ocamlc_config var
let ocaml_config_var_exn t var =
  get_ocaml_config_var_exn t.ocamlc_config var
    ~ocamlc_config_cmd:t.ocamlc_config_cmd

let create ?dest_dir ?ocamlc ?(log=ignore) name =
  let dest_dir =
    match dest_dir with
    | Some dir -> dir
    | None -> Temp.create_temp_dir ~prefix:"ocaml-configurator" ~suffix:""
  in
  let ocamlc =
    match ocamlc with
    | Some fn -> fn
    | None -> Find_in_path.find_ocaml_prog "ocamlc"
  in
  let ocamlc_config_cmd = command_line ocamlc ["-config"] in
  let t =
    { name
    ; ocamlc
    ; log
    ; dest_dir
    ; counter = 0
    ; ext_obj       = ""
    ; c_compiler    = ""
    ; stdlib_dir    = ""
    ; ccomp_type    = ""
    ; ocamlc_config = Map.empty (module String)
    ; ocamlc_config_cmd
    }
  in
  let ocamlc_config =
    let colon_space = String.Search_pattern.create ": " in
    run_capture_exn t ~dir:dest_dir ocamlc_config_cmd
    |> String.split_lines
    |>  List.map ~f:(fun line ->
      match String.Search_pattern.index colon_space ~in_:line with
      | Some i ->
        (String.sub line ~pos:0 ~len:i,
         String.sub line ~pos:(i + 2) ~len:(String.length line - i - 2))
      | None ->
        die "unrecognized line in the output of `%s`: %s" ocamlc_config_cmd
          line)
    |> Map.of_alist (module String)
    |> function
    | `Ok x -> x
    | `Duplicate_key key ->
      die "variable %S present twice in the output of `%s`" key ocamlc_config_cmd
  in
  let get = get_ocaml_config_var_exn ocamlc_config ~ocamlc_config_cmd in
  let c_compiler =
    match Map.find ocamlc_config "c_compiler" with
    | Some c_comp -> c_comp ^ " " ^ get "ocamlc_cflags"
    | None -> get "bytecomp_c_compiler"
  in
  { t with
    ocamlc_config
  ; ext_obj    = get "ext_obj"
  ; c_compiler
  ; stdlib_dir = get "standard_library"
  ; ccomp_type = get "ccomp_type"
  }

let need_to_compile_and_link_separately t =
  (* Vague memory from writing the discover.ml script for Lwt... *)
  match t.ccomp_type with
  | "msvc" -> true
  | _      -> false

let compile_c_prog t ?(c_flags=[]) ?(link_flags=[]) code =
  let dir = t.dest_dir ^/ sprintf "c-test-%d" (gen_id t) in
  Unix.mkdir dir 0o777;
  let base = dir ^/ "test" in
  let c_fname = base ^ ".c" in
  let obj_fname = base ^ t.ext_obj in
  let exe_fname = base ^ ".exe" in
  Out_channel.write_all c_fname ~data:code;
  logf t "compiling c program:";
  List.iter (String.split_lines code) ~f:(logf t " | %s");
  let run_ok args =
    run_ok t ~dir
      (String.concat ~sep:" "
         (t.c_compiler :: List.map args ~f:Fn.quote))
  in
  let ok =
    if need_to_compile_and_link_separately t then
      run_ok (c_flags @ ["-I"; t.stdlib_dir; "-c"; c_fname]) &&
      run_ok ("-o" :: exe_fname :: obj_fname :: link_flags)
    else
      run_ok
        (List.concat
           [ c_flags
           ; [ "-I"; t.stdlib_dir
             ; "-o"; exe_fname
             ; c_fname
             ]
           ; link_flags
           ])
  in
  if ok then Ok exe_fname else Error ()

let c_test t ?c_flags ?link_flags code =
  match compile_c_prog t ?c_flags ?link_flags code with
  | Ok    _ -> true
  | Error _ -> false

module C_define = struct
  module Type = struct
    type t =
      | Switch
      | Int
      | String

    let compare x y =
      match x, y with
      | Switch, Switch -> 0
      | Int, Int -> 0
      | String, String -> 0
      | Switch, (Int | String) -> 1
      | (Int | String), Switch -> -1
      | Int, String -> 1
      | String, Int -> -1

    let sexp_of_t = function
      | Switch -> Sexp.Atom "switch"
      | Int -> Sexp.Atom "int"
      | String -> Sexp.Atom "string"

    let t_of_sexp = function
      | Sexp.Atom "switch" -> Switch
      | Sexp.Atom "int" -> Int
      | Sexp.Atom "string" -> String
      | s -> raise (Sexp.Of_sexp_error (Failure "C_define.Type.t_of_sexp", s))
  end

  module Value = struct
    type t =
      | Switch of bool
      | Int    of int
      | String of string

    let compare x y =
      match x, y with
      | Switch x, Switch y -> Bool.compare x y
      | Int x, Int y -> Int.compare x y
      | String x, String y -> String.compare x y
      | Switch _, (Int _ | String _) -> 1
      | (Int _ | String _), Switch _ -> -1
      | Int _, String _ -> 1
      | String _, Int _ -> -1

    let sexp_of_t =
      let open Sexp in
      function
      | Switch b -> List [Atom "switch"; Bool.sexp_of_t b]
      | Int i -> List [Atom "int"; Int.sexp_of_t i]
      | String s -> List [Atom "string"; String.sexp_of_t s]

    let t_of_sexp =
      let open Sexp in
      function
      | List [Atom "switch"; x] -> Switch (Bool.t_of_sexp x)
      | List [Atom "int"; x] -> Int (Int.t_of_sexp x)
      | List [Atom "string"; x] -> String (String.t_of_sexp x)
      | s -> raise (Sexp.Of_sexp_error (Failure "C_define.Value.t_of_sexp", s))
  end

  let import t ?c_flags ?link_flags ~includes vars =
    let buf = Buffer.create 1024 in
    let pr fmt = Printf.bprintf buf (fmt ^^ "\n") in
    let includes = "stdio.h" :: includes in
    List.iter includes ~f:(pr "#include <%s>");
    pr "";
    pr "int main()";
    pr "{";
    List.iter vars ~f:(fun (name, (kind : Type.t)) ->
      match kind with
      | Switch ->
        pr {|#if defined(%s)|} name;
        pr {|  printf("%s=b:true\n");|} name;
        pr {|#else|};
        pr {|  printf("%s=b:false\n");|} name;
        pr {|#endif|}
      | Int ->
        pr {|  printf("%s=i:%%d\n", %s);|} name name
      | String ->
        pr {|  printf("%s=s:%%s\n", %s);|} name name);
    pr "  return 0;";
    pr "}";
    let code = Buffer.contents buf in
    match compile_c_prog t ?c_flags ?link_flags code with
    | Error () -> die "failed to compile program"
    | Ok exe ->
      run_capture_exn t ~dir:(Fn.dirname exe) (command_line exe [])
      |> String.split_lines
      |> List.map ~f:(fun s : (string * Value.t) ->
        let var, data = String.lsplit2_exn s ~on:'=' in
        (var,
         match String.lsplit2_exn data ~on:':' with
         | "b", s -> Switch (Bool.of_string s)
         | "i", s -> Int    (Int. of_string s)
         | "s", s -> String s
         | _ -> assert false))

  let gen_header_file t ~fname ?protection_var vars =
    let protection_var =
      match protection_var with
      | Some v -> v
      | None ->
        String.map (t.name ^ "_" ^ Fn.basename fname) ~f:(function
          | 'a'..'z' as c -> Char.uppercase c
          | 'A'..'Z' | '0'..'9' as c -> c
          | _ -> '_')
    in
    let vars = List.sort vars ~cmp:(fun (a, _) (b, _) -> String.compare a b) in
    let lines =
      List.map vars ~f:(fun (name, value) ->
        match (value : Value.t) with
        | Switch false -> sprintf "#undef  %s" name
        | Switch true  -> sprintf "#define %s" name
        | Int    n     -> sprintf "#define %s (%d)" name n
        | String s     -> sprintf "#define %s %S" name s)
    in
    let lines =
      List.concat
        [ [ sprintf "#ifndef %s" protection_var
          ; sprintf "#define %s" protection_var
          ]
        ; lines
        ; [ "#endif" ]
        ]
    in
    logf t "writing header file %s" fname;
    List.iter lines ~f:(logf t " | %s");
    let tmp_fname = fname ^ ".tmp" in
    Out_channel.write_lines tmp_fname lines;
    Sys.rename tmp_fname fname
end


let find_in_path t prog =
  logf t "find_in_path: %s" prog;
  let x = Find_in_path.find prog in
  logf t "-> %s"
    (match x with
     | None -> "not found"
     | Some fn -> "found: " ^ quote fn);
  x

module Pkg_config = struct
  type nonrec t =
    { pkg_config   : string
    ; configurator : t
    }

  let get c =
    Option.map (find_in_path c "pkg-config") ~f:(fun pkg_config ->
      { pkg_config; configurator = c })

  type package_conf =
    { libs   : string list
    ; cflags : string list
    }

  let query t ~package =
    let package = quote package in
    let pkg_config = quote t.pkg_config in
    let c = t.configurator in
    let dir = c.dest_dir in
    let env =
      match ocaml_config_var c "system" with
      | Some "macosx" -> begin
          match find_in_path c "brew" with
          | Some brew ->
            let prefix =
              String.strip (run_capture_exn c ~dir (command_line brew ["--prefix"]))
            in
            sprintf "env PKG_CONFIG_PATH=%s/opt/%s/lib/pkgconfig:$PKG_CONFIG_PATH "
              (quote prefix) package
          | None ->
            ""
        end
      | _ -> ""
    in
    if run_ok c ~dir (sprintf "%s%s %s" env pkg_config package) then
      let run what =
        match
          String.strip
            (run_capture_exn c ~dir (sprintf "%s%s %s %s" env pkg_config what package))
        with
        | "" -> []
        | s  -> String.split s ~on:' '
      in
      Some
        { libs   = run "--libs"
        ; cflags = run "--cflags"
        }
    else
      None
end

let main ?(args=[]) ~name f =
  let ocamlc  = ref None  in
  let verbose = ref false in
  let dest_dir = ref None in
  let args =
    Arg.align
      ([ "-ocamlc", Arg.String (fun s -> ocamlc := Some s),
         "PATH ocamlc command to use"
       ; "-verbose", Arg.Set verbose,
         " be verbose"
       ; "-dest-dir", Arg.String (fun s -> dest_dir := Some s),
         "DIR save temporary files to this directory"
       ] @ args)
  in
  let anon s = raise (Arg.Bad (sprintf "don't know what to do with %s" s)) in
  let usage = sprintf "%s [OPTIONS]" (Fn.basename Sys.executable_name) in
  Arg.parse args anon usage;
  let log_db = ref [] in
  let log s = log_db := s :: !log_db in
  let t =
    create
      ?dest_dir:!dest_dir
      ?ocamlc:!ocamlc
      ~log:(if !verbose then prerr_endline else log)
      name
  in
  try
    f t
  with exn ->
    List.iter (List.rev !log_db) ~f:(eprintf "%s\n");
    match exn with
    | Fatal_error msg ->
      eprintf "Error: %s\n%!" msg;
      Caml.exit 1
    | exn -> raise exn
