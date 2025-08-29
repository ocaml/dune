(** {2 Command line} *)

let concurrency, verbose, debug, secondary, force_byte_compilation, static, build_dir =
  let build_dir = ref "_boot" in
  let anon s = raise (Arg.Bad (Printf.sprintf "don't know what to do with %s\n" s)) in
  let concurrency = ref None in
  let verbose = ref false in
  let prog = Filename.basename Sys.argv.(0) in
  let debug = ref false in
  let secondary = ref false in
  let force_byte_compilation = ref false in
  let static = ref false in
  Arg.parse
    [ "-j", Int (fun n -> concurrency := Some n), "JOBS Concurrency"
    ; "--verbose", Set verbose, " Set the display mode"
    ; "--keep-generated-files", Unit ignore, " Keep generated files"
    ; "--debug", Set debug, " Enable various debugging options"
    ; "--secondary", Set secondary, " Use the secondary compiler installation"
    ; ( "--force-byte-compilation"
      , Set force_byte_compilation
      , " Force bytecode compilation even if ocamlopt is available" )
    ; "--static", Set static, " Build a static binary"
    ; "--boot-dir", Set_string build_dir, " Set the boot directory"
    ]
    anon
    (Printf.sprintf "Usage: %s <options>\nOptions are:" prog);
  !concurrency, !verbose, !debug, !secondary, !force_byte_compilation, !static, !build_dir
;;

(** {2 General configuration} *)

type task =
  { target : string * string
  ; external_libraries : string list
  ; local_libraries : Libs.library list
  }

let task =
  { target = "dune", "bin/main.ml"
  ; external_libraries = Libs.external_libraries
  ; local_libraries = Libs.local_libraries
  }
;;

(** {2 Utility functions} *)

open StdLabels
open Printf

module String = struct
  include String
  module Set = Set.Make (String)
  module Map = Map.Make (String)

  let is_suffix t ~suffix = Filename.check_suffix t suffix

  let is_prefix t ~prefix =
    let len_s = length t
    and len_pre = length prefix in
    let rec aux i =
      if i = len_pre
      then true
      else if unsafe_get t i <> unsafe_get prefix i
      then false
      else aux (i + 1)
    in
    len_s >= len_pre && aux 0
  ;;
end

module List = struct
  include List

  let partition_map_skip t ~f =
    let rec loop l m r = function
      | [] -> l, m, r
      | x :: xs ->
        (match f x with
         | `Skip -> loop l m r xs
         | `Left x -> loop (x :: l) m r xs
         | `Middle x -> loop l (x :: m) r xs
         | `Right x -> loop l m (x :: r) xs)
    in
    let l, m, r = loop [] [] [] t in
    rev l, rev m, rev r
  ;;

  let rec filter_map l ~f =
    match l with
    | [] -> []
    | x :: l ->
      (match f x with
       | None -> filter_map l ~f
       | Some x -> x :: filter_map l ~f)
  ;;
end

let ( ^/ ) = Filename.concat

let fatal fmt =
  ksprintf
    (fun s ->
       prerr_endline s;
       exit 2)
    fmt
;;

module Status_line = struct
  let num_jobs = ref 0
  let num_jobs_finished = ref 0
  let displayed = ref ""

  let display_status_line =
    Unix.(isatty stdout)
    ||
    match Sys.getenv "INSIDE_EMACS" with
    | (_ : string) -> true
    | exception Not_found -> false
  ;;

  let update jobs =
    if display_status_line && !num_jobs > 0
    then (
      let new_displayed =
        sprintf "Done: %d/%d (jobs: %d)" !num_jobs_finished !num_jobs jobs
      in
      Printf.printf "\r%*s\r%s%!" (String.length !displayed) "" new_displayed;
      displayed := new_displayed)
  ;;

  let clear () = Printf.printf "\r*s\r%!"
  let () = at_exit (fun () -> Printf.printf "\r%*s\r" (String.length !displayed) "")
end

(* Return a sorted list of entries in [path] as [path/entry] *)
let readdir path =
  Sys.readdir path
  |> Array.to_list
  |> List.map ~f:(fun entry -> path ^/ entry)
  |> List.sort ~cmp:String.compare
;;

let open_out file =
  if Sys.file_exists file then fatal "%s already exists" file;
  open_out file
;;

let input_lines ic =
  let rec loop ic acc =
    match input_line ic with
    | line -> loop ic (line :: acc)
    | exception End_of_file -> List.rev acc
  in
  loop ic []
;;

let read_lines fn =
  let ic = open_in fn in
  let lines = input_lines ic in
  close_in ic;
  lines
;;

let read_file fn =
  let ic = open_in_bin fn in
  let s = really_input_string ic (in_channel_length ic) in
  close_in ic;
  s
;;

let split_lines s =
  let rec loop ~last_is_cr ~acc i j =
    if j = String.length s
    then (
      let acc =
        if j = i || (j = i + 1 && last_is_cr)
        then acc
        else String.sub s ~pos:i ~len:(j - i) :: acc
      in
      List.rev acc)
    else (
      match s.[j] with
      | '\r' -> loop ~last_is_cr:true ~acc i (j + 1)
      | '\n' ->
        let line =
          let len = if last_is_cr then j - i - 1 else j - i in
          String.sub s ~pos:i ~len
        in
        loop ~acc:(line :: acc) (j + 1) (j + 1) ~last_is_cr:false
      | _ -> loop ~acc i (j + 1) ~last_is_cr:false)
  in
  loop ~acc:[] 0 0 ~last_is_cr:false
;;

let do_then_copy ~f a b =
  if Sys.file_exists b then fatal "%s already exists" b;
  let ic = open_in_bin a in
  let len = in_channel_length ic in
  let s = really_input_string ic len in
  close_in ic;
  let oc = open_out_bin b in
  f oc;
  output_string oc s;
  close_out oc
;;

(* copy a file - fails if the file exists *)
let copy a b = do_then_copy ~f:(fun _ -> ()) a b

(* copy a file and insert a header - fails if the file exists *)
let copy_with_header ~header a b = do_then_copy ~f:(fun oc -> output_string oc header) a b

(* copy a file and insert a directive - fails if the file exists *)
let copy_with_directive ~directive a b =
  do_then_copy ~f:(fun oc -> fprintf oc "#%s 1 %S\n" directive a) a b
;;

let path_sep = if Sys.win32 then ';' else ':'

let split_path s =
  let rec loop i j =
    if j = String.length s
    then [ String.sub s ~pos:i ~len:(j - i) ]
    else if s.[j] = path_sep
    then String.sub s ~pos:i ~len:(j - i) :: loop (j + 1) (j + 1)
    else loop i (j + 1)
  in
  loop 0 0
;;

let path =
  match Sys.getenv "PATH" with
  | exception Not_found -> []
  | s -> split_path s
;;

let find_prog ~f =
  let rec search = function
    | [] -> None
    | dir :: rest ->
      (match f dir with
       | None -> search rest
       | Some fn -> Some (dir, fn))
  in
  search path
;;

let exe = if Sys.win32 then ".exe" else ""

(** {2 Concurrency level} *)

let concurrency =
  let try_run_and_capture_line (prog, args) =
    match
      find_prog ~f:(fun dir -> if Sys.file_exists (dir ^/ prog) then Some prog else None)
    with
    | None -> None
    | Some (dir, prog) ->
      let path = dir ^/ prog in
      let args = Array.of_list @@ (path :: args) in
      let ic, oc, ec = Unix.open_process_args_full path args (Unix.environment ()) in
      let line =
        match input_line ic with
        | s -> Some s
        | exception End_of_file -> None
      in
      (match Unix.close_process_full (ic, oc, ec), line with
       | WEXITED 0, Some s -> Some s
       | _ -> None)
  in
  match concurrency with
  | Some n -> n
  | None ->
    (* If no [-j] was given, try to autodetect the number of processors *)
    if Sys.win32
    then (
      match Sys.getenv_opt "NUMBER_OF_PROCESSORS" with
      | None -> 1
      | Some s ->
        (match int_of_string s with
         | exception _ -> 1
         | n -> n))
    else (
      let commands =
        [ "nproc", []
        ; "getconf", [ "_NPROCESSORS_ONLN" ]
        ; "getconf", [ "NPROCESSORS_ONLN" ]
        ]
      in
      let rec loop = function
        | [] -> 1
        | cmd :: rest ->
          (match try_run_and_capture_line cmd with
           | None -> loop rest
           | Some s ->
             (match int_of_string (String.trim s) with
              | n -> n
              | exception _ -> loop rest))
      in
      loop commands)
;;

(** {2 Fibers} *)

module Fiber : sig
  (** Fibers *)

  (** This module is similar to the one in [../src/fiber] except that it is much
      less optimised and much easier to understand. You should look at the
      documentation of the other module to understand the API. *)

  type 'a t

  val return : 'a -> 'a t

  module O : sig
    val ( >>> ) : unit t -> 'a t -> 'a t
    val ( >>= ) : 'a t -> ('a -> 'b t) -> 'b t
    val ( >>| ) : 'a t -> ('a -> 'b) -> 'b t
  end

  module Future : sig
      type 'a fiber
      type 'a t

      val wait : 'a t -> 'a fiber
    end
    with type 'a fiber := 'a t

  val fork : (unit -> 'a t) -> 'a Future.t t
  val fork_and_join : (unit -> 'a t) -> (unit -> 'b t) -> ('a * 'b) t
  val fork_and_join_unit : (unit -> unit t) -> (unit -> 'a t) -> 'a t
  val parallel_map : 'a list -> f:('a -> 'b t) -> 'b list t
  val parallel_iter : 'a list -> f:('a -> unit t) -> unit t

  module Process : sig
    val run : ?cwd:string -> string -> string list -> unit t
    val run_and_capture : ?cwd:string -> string -> string list -> string t
    val try_run_and_capture : ?cwd:string -> string -> string list -> string option t
  end

  val run : 'a t -> 'a
end = struct
  open MoreLabels

  type 'a t = ('a -> unit) -> unit

  let return x k = k x

  module O = struct
    let ( >>> ) a b k = a (fun () -> b k)
    let ( >>= ) t f k = t (fun x -> f x k)
    let ( >>| ) t f k = t (fun x -> k (f x))
  end

  open O

  let both a b = a >>= fun a -> b >>= fun b -> return (a, b)

  module Ivar = struct
    type 'a state =
      | Full of 'a
      | Empty of ('a -> unit) Queue.t

    type 'a t = { mutable state : 'a state }

    let create () = { state = Empty (Queue.create ()) }

    let fill t x =
      match t.state with
      | Full _ -> failwith "Fiber.Ivar.fill"
      | Empty q ->
        t.state <- Full x;
        Queue.iter (fun f -> f x) q
    ;;

    let read t k =
      match t.state with
      | Full x -> k x
      | Empty q -> Queue.push k q
    ;;
  end

  module Future = struct
    type 'a t = 'a Ivar.t

    let wait = Ivar.read
  end

  let fork f k =
    let ivar = Ivar.create () in
    f () (fun x -> Ivar.fill ivar x);
    k ivar
  ;;

  let fork_and_join f g =
    fork f >>= fun a -> fork g >>= fun b -> both (Future.wait a) (Future.wait b)
  ;;

  let fork_and_join_unit f g =
    fork f >>= fun a -> fork g >>= fun b -> Future.wait a >>> Future.wait b
  ;;

  let rec parallel_map l ~f =
    match l with
    | [] -> return []
    | x :: l ->
      fork (fun () -> f x)
      >>= fun future ->
      parallel_map l ~f >>= fun l -> Future.wait future >>= fun x -> return (x :: l)
  ;;

  let rec parallel_iter l ~f =
    match l with
    | [] -> return ()
    | x :: l ->
      fork (fun () -> f x)
      >>= fun future -> parallel_iter l ~f >>= fun () -> Future.wait future
  ;;

  module Temp = struct
    module Files = Set.Make (String)

    let tmp_files = ref Files.empty

    let () =
      at_exit (fun () ->
        let fns = !tmp_files in
        tmp_files := Files.empty;
        Files.iter fns ~f:(fun fn ->
          try Sys.remove fn with
          | _ -> ()))
    ;;

    let file prefix suffix =
      let fn = Filename.temp_file prefix suffix in
      tmp_files := Files.add fn !tmp_files;
      fn
    ;;

    let destroy_file fn =
      (try Sys.remove fn with
       | _ -> ());
      tmp_files := Files.remove fn !tmp_files
    ;;
  end

  module Process = struct
    let running = Hashtbl.create concurrency

    exception Finished of int * Unix.process_status

    let rec wait_win32 () =
      match
        Hashtbl.iter running ~f:(fun ~key:pid ~data:_ ->
          let pid, status = Unix.waitpid [ WNOHANG ] pid in
          if pid <> 0 then raise_notrace (Finished (pid, status)))
      with
      | () ->
        ignore (Unix.select [] [] [] 0.001);
        wait_win32 ()
      | exception Finished (pid, status) -> pid, status
    ;;

    let wait = if Sys.win32 then wait_win32 else Unix.wait
    let waiting_for_slot = Queue.create ()

    let throttle () =
      if Hashtbl.length running >= concurrency
      then (
        let ivar = Ivar.create () in
        Queue.push ivar waiting_for_slot;
        Ivar.read ivar)
      else return ()
    ;;

    let restart_throttled () =
      while
        Hashtbl.length running < concurrency && not (Queue.is_empty waiting_for_slot)
      do
        Ivar.fill (Queue.pop waiting_for_slot) ()
      done
    ;;

    let open_temp_file () =
      let out = Temp.file "duneboot-" ".output" in
      let fd =
        Unix.openfile out [ O_WRONLY; O_CREAT; O_TRUNC; O_SHARE_DELETE; O_CLOEXEC ] 0o666
      in
      out, fd
    ;;

    let read_temp fn =
      let s = read_file fn in
      Temp.destroy_file fn;
      s
    ;;

    let initial_cwd = Sys.getcwd ()

    let run_process ?cwd prog args ~split =
      throttle ()
      >>= fun () ->
      let stdout_fn, stdout_fd = open_temp_file () in
      let stderr_fn, stderr_fd =
        if split then open_temp_file () else stdout_fn, stdout_fd
      in
      (match cwd with
       | Some x -> Sys.chdir x
       | None -> ());
      let pid =
        Unix.create_process
          prog
          (Array.of_list (prog :: args))
          Unix.stdin
          stdout_fd
          stderr_fd
      in
      (match cwd with
       | Some _ -> Sys.chdir initial_cwd
       | None -> ());
      Unix.close stdout_fd;
      if split then Unix.close stderr_fd;
      let ivar = Ivar.create () in
      Hashtbl.add running ~key:pid ~data:ivar;
      Ivar.read ivar
      >>= fun (status : Unix.process_status) ->
      let stdout_s = read_temp stdout_fn in
      let stderr_s = if split then read_temp stderr_fn else stdout_s in
      if stderr_s <> "" || status <> WEXITED 0 || verbose
      then (
        let cmdline = String.concat ~sep:" " (prog :: args) in
        let cmdline =
          match cwd with
          | Some x -> sprintf "cd %s && %s" x cmdline
          | None -> cmdline
        in
        Status_line.clear ();
        prerr_endline cmdline;
        prerr_string stderr_s;
        flush stderr);
      match status with
      | WEXITED 0 -> return (Ok stdout_s)
      | WEXITED n -> return (Error n)
      | WSIGNALED _ -> return (Error 255)
      | WSTOPPED _ -> assert false
    ;;

    let run ?cwd prog args =
      run_process ?cwd prog args ~split:false
      >>| function
      | Ok _ -> ()
      | Error n -> exit n
    ;;

    let run_and_capture ?cwd prog args =
      run_process ?cwd prog args ~split:true
      >>| function
      | Ok x -> x
      | Error n -> exit n
    ;;

    let try_run_and_capture ?cwd prog args =
      run_process ?cwd prog args ~split:true
      >>| function
      | Ok x -> Some x
      | Error _ -> None
    ;;
  end

  let run t =
    let result = ref None in
    t (fun x -> result := Some x);
    let rec loop () =
      if Hashtbl.length Process.running > 0
      then (
        Status_line.update (Hashtbl.length Process.running);
        let pid, status = Process.wait () in
        let ivar = Hashtbl.find Process.running pid in
        Hashtbl.remove Process.running pid;
        Ivar.fill ivar status;
        Process.restart_throttled ();
        loop ())
      else (
        match !result with
        | Some x -> x
        | None -> fatal "bootstrap got stuck!")
    in
    loop ()
  ;;
end

open Fiber.O
module Process = Fiber.Process

(** {2 OCaml tools} *)

module Mode = struct
  type t =
    | Byte
    | Native
end

module Config : sig
  val compiler : string
  val ocamldep : string
  val ocamllex : string
  val ocamlyacc : string
  val mode : Mode.t
  val ocaml_archive_ext : string
  val ocaml_config : unit -> string String.Map.t Fiber.t
  val output_complete_obj_arg : string
  val unix_library_flags : string list
end = struct
  let ocaml_version = Scanf.sscanf Sys.ocaml_version "%d.%d" (fun a b -> a, b)
  let prog_not_found prog = fatal "Program %s not found in PATH" prog

  let best_prog dir prog =
    let fn = dir ^/ prog ^ ".opt" ^ exe in
    if Sys.file_exists fn
    then Some fn
    else (
      let fn = dir ^/ prog ^ exe in
      if Sys.file_exists fn then Some fn else None)
  ;;

  let find_prog prog = find_prog ~f:(fun dir -> best_prog dir prog)

  let get_prog dir prog =
    match best_prog dir prog with
    | None -> prog_not_found prog
    | Some fn -> fn
  ;;

  let bin_dir, ocamlc =
    if secondary
    then (
      let s =
        Fiber.run
          (Process.run_and_capture
             "ocamlfind"
             [ "-toolchain"; "secondary"; "query"; "ocaml" ])
      in
      match split_lines s with
      | [] | _ :: _ :: _ -> fatal "Unexpected output locating secondary compiler"
      | [ bin_dir ] ->
        (match best_prog bin_dir "ocamlc" with
         | None -> fatal "Failed to locate secondary ocamlc"
         | Some x -> bin_dir, x))
    else (
      match find_prog "ocamlc" with
      | None -> prog_not_found "ocamlc"
      | Some x -> x)
  ;;

  let ocamlyacc = get_prog bin_dir "ocamlyacc"
  let ocamllex = get_prog bin_dir "ocamllex"
  let ocamldep = get_prog bin_dir "ocamldep"

  let compiler, mode, ocaml_archive_ext =
    match force_byte_compilation, best_prog bin_dir "ocamlopt" with
    | true, _ | _, None -> ocamlc, Mode.Byte, ".cma"
    | false, Some path -> path, Mode.Native, ".cmxa"
  ;;

  let ocaml_config () =
    Process.run_and_capture ocamlc [ "-config" ]
    >>| fun s ->
    List.fold_left (split_lines s) ~init:String.Map.empty ~f:(fun acc line ->
      match Scanf.sscanf line "%[^:]: %s" (fun k v -> k, v) with
      | k, v -> String.Map.add k v acc
      | exception _ ->
        fatal "invalid line in output of 'ocamlc -config': %s" (String.escaped line))
  ;;

  let output_complete_obj_arg =
    if ocaml_version < (4, 10) then "-custom" else "-output-complete-exe"
  ;;

  let unix_library_flags = if ocaml_version >= (5, 0) then [ "-I"; "+unix" ] else []
end

let insert_header fn ~header =
  match header with
  | "" -> ()
  | h ->
    let s = read_file fn in
    let oc = open_out_bin fn in
    output_string oc h;
    output_string oc s;
    close_out oc
;;

let copy_lexer ~header src dst =
  let dst = Filename.remove_extension dst ^ ".ml" in
  Process.run Config.ocamllex [ "-q"; "-o"; dst; src ]
  >>| fun () -> insert_header dst ~header
;;

let copy_parser ~header src dst =
  let dst = Filename.remove_extension dst in
  Process.run Config.ocamlyacc [ "-b"; dst; src ]
  >>| fun () ->
  insert_header (dst ^ ".ml") ~header;
  insert_header (dst ^ ".mli") ~header
;;

(** {2 Handling of the dune-build-info library} *)

(** {2 Preparation of library files} *)
module Build_info = struct
  let get_version () =
    let from_dune_project =
      match read_lines "dune-project" with
      | exception _ -> None
      | lines ->
        let rec loop = function
          | [] -> None
          | line :: lines ->
            (match Scanf.sscanf line "(version %[^)])" (fun v -> v) with
             | exception _ -> loop lines
             | v -> Some v)
        in
        loop lines
    in
    match from_dune_project with
    | Some _ -> Fiber.return from_dune_project
    | None ->
      if not (Sys.file_exists ".git")
      then Fiber.return None
      else
        Process.try_run_and_capture
          "git"
          [ "describe"; "--always"; "--dirty"; "--abbrev=7" ]
        >>| (function
         | Some s -> Some (String.trim s)
         | None -> None)
  ;;

  let gen_data_module oc =
    let pr fmt = fprintf oc fmt in
    let prlist name l ~f =
      match l with
      | [] -> pr "let %s = []\n" name
      | x :: l ->
        pr "let %s =\n" name;
        pr "  [ ";
        f x;
        List.iter l ~f:(fun x ->
          pr "  ; ";
          f x);
        pr "  ]\n"
    in
    get_version ()
    >>| fun version ->
    pr
      "let version = %s\n"
      (match version with
       | None -> "None"
       | Some v -> sprintf "Some %S" v);
    pr "\n";
    let libs =
      List.map task.local_libraries ~f:(fun (lib : Libs.library) -> lib.path, "version")
      @ List.map task.external_libraries ~f:(fun name ->
        name, {|Some "[distributed with OCaml]"|})
      |> List.sort ~cmp:(fun (a, _) (b, _) -> String.compare a b)
    in
    prlist "statically_linked_libraries" libs ~f:(fun (name, v) -> pr "%S, %s\n" name v)
  ;;
end

(* module OCaml_file = struct module Kind = struct type t = Impl | Intf end

   type t = { kind : Kind.t ; module_name = end *)

module Library = struct
  module File_kind = struct
    type asm =
      { syntax : [ `Gas | `Intel ]
      ; arch : [ `Amd64 ] option
      ; os : [ `Win | `Unix ] option
      ; assembler : [ `C_comp | `Msvc_asm ]
      }

    type c =
      { arch : [ `Arm64 | `X86 ] option
      ; flags : string list
      }

    type t =
      | Header
      | C of c
      | Asm of asm
      | Ml
      | Mli
      | Mll
      | Mly

    let analyse file =
      let fn = Filename.basename file in
      let i =
        try String.index fn '.' with
        | Not_found -> String.length fn
      in
      match String.sub fn ~pos:i ~len:(String.length fn - i) with
      | (".S" | ".asm") as ext ->
        let syntax = if ext = ".S" then `Gas else `Intel in
        let os, arch, assembler =
          let fn = Filename.remove_extension fn in
          let check suffix = String.is_suffix fn ~suffix in
          if check "x86-64_unix"
          then Some `Unix, Some `Amd64, `C_comp
          else if check "x86-64_windows_gnu"
          then Some `Win, Some `Amd64, `C_comp
          else if check "x86-64_windows_msvc"
          then Some `Win, Some `Amd64, `Msvc_asm
          else None, None, `C_comp
        in
        Some (Asm { syntax; arch; os; assembler })
      | ".c" ->
        let arch, flags =
          let fn = Filename.remove_extension fn in
          let check suffix = String.is_suffix fn ~suffix in
          let x86 gnu _msvc =
            (* CR rgrinberg: select msvc flags on windows *)
            Some `X86, gnu
          in
          if check "_sse2"
          then x86 [ "-msse2" ] [ "/arch:SSE2" ]
          else if check "_sse41"
          then x86 [ "-msse4.1" ] [ "/arch:AVX" ]
          else if check "_avx2"
          then x86 [ "-mavx2" ] [ "/arch:AVX2" ]
          else if check "_avx512"
          then x86 [ "-mavx512f"; "-mavx512vl"; "-mavx512bw" ] [ "/arch:AVX512" ]
          else if String.is_suffix fn ~suffix:"_neon"
          then Some `Arm64, []
          else None, []
        in
        Some (C { arch; flags })
      | ".h" -> Some Header
      | ".ml" -> Some Ml
      | ".mli" -> Some Mli
      | ".mll" -> Some Mll
      | ".mly" -> Some Mly
      | ".defaults.ml" ->
        let fn' = String.sub fn ~pos:0 ~len:i ^ ".ml" in
        let dn = Filename.dirname file in
        if Sys.file_exists (dn ^/ fn') then None else Some Ml
      | _ -> None
    ;;
  end

  type source =
    { file : string
    ; kind : File_kind.t
    }

  type c_file =
    { name : string
    ; flags : string list
    }

  type asm_file =
    { assembler : [ `C_comp | `Msvc_asm ]
    ; flags : string list
    ; out_file : string
    }

  module Wrapper = struct
    type t =
      { toplevel_module : string
      ; alias_module : string
      }

    let make ~namespace ~modules =
      match namespace with
      | None -> None
      | Some namespace ->
        let namespace = String.capitalize_ascii namespace in
        if String.Set.equal modules (String.Set.singleton namespace)
        then None
        else if String.Set.mem namespace modules
        then Some { toplevel_module = namespace; alias_module = namespace ^ "__" }
        else Some { toplevel_module = namespace; alias_module = namespace }
    ;;

    let mangle_filename t ({ file; kind } : source) =
      let base = Filename.basename file in
      match kind with
      | Asm _ | C _ | Header -> base
      | Mll | Mly | Ml | Mli ->
        let ext =
          match kind with
          | Mli -> ".mli"
          | _ -> ".ml"
        in
        let base =
          String.sub base ~pos:0 ~len:(String.index base '.') |> String.uncapitalize_ascii
        in
        (match t with
         | None -> base ^ ext
         | Some t ->
           if String.capitalize_ascii base = t.toplevel_module
           then base ^ ext
           else (
             let base = String.capitalize_ascii base in
             String.uncapitalize_ascii t.toplevel_module ^ "__" ^ base ^ ext))
    ;;

    let header t =
      match t with
      | None -> ""
      | Some t -> sprintf "open! %s\n" t.alias_module
    ;;

    let generate_wrapper t modules =
      match t with
      | None -> None
      | Some t ->
        let fn = String.uncapitalize_ascii t.alias_module ^ ".ml" in
        let oc = open_out (build_dir ^/ fn) in
        String.Set.iter
          (fun m ->
             if m <> t.toplevel_module
             then fprintf oc "module %s = %s__%s\n" m t.toplevel_module m)
          modules;
        close_out oc;
        Some fn
    ;;
  end

  (* Collect source files *)
  let scan ~dir ~scan_subdirs =
    let rec loop files acc =
      match files with
      | [] -> acc
      | file :: files ->
        let acc =
          if Sys.is_directory file
          then if scan_subdirs then loop (readdir file) acc else acc
          else (
            match File_kind.analyse file with
            | Some kind -> { file; kind } :: acc
            | None -> acc)
        in
        loop files acc
    in
    loop (readdir dir) []
  ;;

  type t =
    { ocaml_files : string list
    ; alias_file : string option
    ; c_files : c_file list
    ; asm_files : asm_file list
    }

  let keep_asm
        { File_kind.syntax; arch; os; assembler = _ }
        ~ccomp_type
        ~architecture
        ~os_type
    =
    (match os with
     | Some `Unix -> String.equal os_type "Unix"
     | Some `Win -> String.equal os_type "Win32"
     | None -> true)
    && (match syntax, ccomp_type with
        | `Intel, "msvc" -> true
        | `Gas, "msvc" -> false
        | `Gas, _ -> true
        | `Intel, _ -> false)
    &&
    match arch, architecture with
    | None, _ -> true
    | Some `Amd64, "amd64" -> true
    | Some `Amd64, _ -> false
  ;;

  let keep_c { File_kind.arch; flags = _ } ~architecture =
    match arch with
    | None -> true
    | Some `Arm64 -> architecture = "arm64"
    | Some `X86 -> architecture = "amd64" || architecture = "x86_64"
  ;;

  let process
        { Libs.path = dir
        ; main_module_name = namespace
        ; include_subdirs_unqualified = scan_subdirs
        ; special_builtin_support = build_info_module
        }
        ~ocaml_config
        ~word_size
        ~os_type
    =
    let files = scan ~dir ~scan_subdirs in
    let modules =
      let modules =
        List.fold_left files ~init:String.Set.empty ~f:(fun acc { file = fn; kind } ->
          match (kind : File_kind.t) with
          | Asm _ | Header | C _ -> acc
          | Ml | Mli | Mll | Mly ->
            let module_name =
              let fn = Filename.basename fn in
              String.sub fn ~pos:0 ~len:(String.index fn '.') |> String.capitalize_ascii
            in
            String.Set.add module_name acc)
      in
      match build_info_module with
      | None -> modules
      | Some m -> String.Set.add (String.capitalize_ascii m) modules
    in
    let wrapper = Wrapper.make ~namespace ~modules in
    let header = Wrapper.header wrapper in
    Fiber.fork_and_join
      (fun () ->
         Fiber.parallel_map files ~f:(fun ({ file = fn; kind } as source) ->
           let mangled = Wrapper.mangle_filename wrapper source in
           let dst = build_dir ^/ mangled in
           (match kind with
            | Asm _ ->
              copy fn dst;
              Fiber.return [ mangled ]
            | Header | C _ ->
              copy_with_directive ~directive:"line" fn dst;
              Fiber.return [ mangled ]
            | Ml | Mli ->
              copy_with_header ~header fn dst;
              Fiber.return [ mangled ]
            | Mll -> copy_lexer fn dst ~header >>> Fiber.return [ mangled ]
            | Mly ->
              (* CR rgrinberg: what if the parser already has an mli? *)
              copy_parser fn dst ~header >>> Fiber.return [ mangled; mangled ^ "i" ])
           >>| function
           | mangled -> List.map mangled ~f:(fun m -> source, m)))
      (fun () ->
         match build_info_module with
         | None -> Fiber.return None
         | Some m ->
           let src =
             let fn = String.uncapitalize_ascii m ^ ".ml" in
             { file = fn; kind = Ml }
           in
           let mangled = Wrapper.mangle_filename wrapper src in
           let oc = open_out (build_dir ^/ mangled) in
           Build_info.gen_data_module oc
           >>| fun () ->
           close_out oc;
           Some (src, mangled))
    >>| fun (files, build_info_file) ->
    let alias_file = Wrapper.generate_wrapper wrapper modules in
    let c_files, ocaml_files, asm_files =
      let files =
        let files = List.concat files in
        match build_info_file with
        | None -> files
        | Some fn -> fn :: files
      in
      let ext_obj =
        try String.Map.find "ext_obj" ocaml_config with
        | Not_found -> ".o"
      in
      let ccomp_type = String.Map.find "ccomp_type" ocaml_config in
      let architecture = String.Map.find "architecture" ocaml_config in
      List.partition_map_skip files ~f:(fun (src, fn) ->
        match src.kind with
        | C c ->
          if keep_c c ~architecture
          then (
            let extra_flags =
              if String.starts_with ~prefix:"blake3_" fn
              then
                if String.equal os_type "Cygwin" || String.equal word_size "32"
                then
                  [ "-DBLAKE3_NO_SSE2"
                  ; "-DBLAKE3_NO_SSE41"
                  ; "-DBLAKE3_NO_AVX2"
                  ; "-DBLAKE3_NO_AVX512"
                  ]
                else []
              else []
            in
            `Left { flags = extra_flags @ c.flags; name = fn })
          else `Skip
        | Ml | Mli | Mly | Mll -> `Middle fn
        | Header -> `Skip
        | Asm asm ->
          if keep_asm asm ~ccomp_type ~architecture ~os_type
          then (
            let out_file = Filename.chop_extension fn ^ ext_obj in
            `Right
              { flags =
                  (match asm.assembler with
                   | `C_comp -> [ "-c"; fn; "-o"; out_file ]
                   | `Msvc_asm -> [ "/nologo"; "/quiet"; "/Fo" ^ out_file; "/c"; fn ])
              ; assembler = asm.assembler
              ; out_file
              })
          else `Skip)
    in
    { ocaml_files; alias_file; c_files; asm_files }
  ;;
end

let ocamldep args =
  Process.run_and_capture Config.ocamldep ("-modules" :: args) ~cwd:build_dir
  >>| fun s ->
  List.map (split_lines s) ~f:(fun line ->
    let colon = String.index line ':' in
    let filename = String.sub line ~pos:0 ~len:colon in
    let modules =
      if colon = String.length line - 1
      then []
      else (
        let modules =
          String.sub line ~pos:(colon + 2) ~len:(String.length line - colon - 2)
        in
        String.split_on_char ~sep:' ' modules)
    in
    filename, modules)
  |> List.sort ~cmp:compare
;;

let mk_flags arg l = List.map l ~f:(fun m -> [ arg; m ]) |> List.flatten

let convert_dependencies ~all_source_files (file, dependencies) =
  let is_mli = Filename.check_suffix file ".mli" in
  let convert_module module_name =
    let filename = String.uncapitalize_ascii module_name in
    if filename = Filename.chop_extension file
    then (* Self-reference *)
      None
    else if String.Set.mem (filename ^ ".mli") all_source_files
    then
      if (not is_mli) && String.Set.mem (filename ^ ".ml") all_source_files
      then
        (* We need to build the .ml for inlining info *)
        Some [ filename ^ ".mli"; filename ^ ".ml" ]
      else (* .mli files never depend on .ml files *)
        Some [ filename ^ ".mli" ]
    else if String.Set.mem (filename ^ ".ml") all_source_files
    then
      (* If there's no .mli, then we must always depend on the .ml *)
      Some [ filename ^ ".ml" ]
    else (* This is a module coming from an external library *)
      None
  in
  let dependencies =
    let dependencies = List.concat (List.filter_map ~f:convert_module dependencies) in
    (* .ml depends on .mli, if it exists *)
    if (not is_mli) && String.Set.mem (file ^ "i") all_source_files
    then (file ^ "i") :: dependencies
    else dependencies
  in
  file, dependencies
;;

let write_args file args =
  let ch = open_out (build_dir ^/ file) in
  output_string ch (String.concat ~sep:"\n" args);
  close_out ch
;;

let get_dependencies libraries =
  let alias_files =
    List.fold_left libraries ~init:[] ~f:(fun acc (lib : Library.t) ->
      match lib.alias_file with
      | None -> acc
      | Some fn -> fn :: acc)
  in
  let all_source_files =
    List.map ~f:(fun (lib : Library.t) -> lib.ocaml_files) libraries |> List.concat
  in
  write_args "source_files" all_source_files;
  ocamldep (mk_flags "-map" alias_files @ [ "-args"; "source_files" ])
  >>| fun dependencies ->
  let all_source_files =
    List.fold_left
      alias_files
      ~init:(String.Set.of_list all_source_files)
      ~f:(fun acc fn -> String.Set.add fn acc)
  in
  let deps =
    List.rev_append
      ((* Alias files have no dependencies *)
       List.rev_map
         alias_files
         ~f:(fun fn -> fn, []))
      (List.rev_map dependencies ~f:(convert_dependencies ~all_source_files))
  in
  if debug
  then (
    eprintf "***** Dependencies *****\n";
    List.iter deps ~f:(fun (fn, deps) ->
      eprintf "%s: %s\n" fn (String.concat deps ~sep:" "));
    eprintf "**********\n");
  deps
;;

let assemble_libraries
      { local_libraries; target = _, main; _ }
      ~ocaml_config
      ~word_size
      ~os_type
  =
  (* In order to assemble all the sources in one place, the executables
       modules are also put in a namespace *)
  let task_lib =
    let dir = Filename.dirname main in
    let namespace =
      String.capitalize_ascii (Filename.chop_extension (Filename.basename main))
    in
    { Libs.path = dir
    ; main_module_name = Some namespace
    ; include_subdirs_unqualified = true
    ; special_builtin_support = None
    }
  in
  local_libraries @ [ task_lib ]
  |> Fiber.parallel_map ~f:(Library.process ~ocaml_config ~word_size ~os_type)
;;

type status =
  | Not_started of (unit -> unit Fiber.t)
  | Initializing
  | Started of unit Fiber.Future.t

let resolve_externals external_libraries =
  let external_libraries, external_includes =
    let convert = function
      | "threads" -> "threads" ^ Config.ocaml_archive_ext, [ "-I"; "+threads" ]
      | "unix" -> "unix" ^ Config.ocaml_archive_ext, Config.unix_library_flags
      | s -> fatal "unhandled external library %s" s
    in
    List.map ~f:convert external_libraries |> List.split
  in
  let external_includes = List.concat external_includes in
  external_libraries, external_includes
;;

let sort_files dependencies ~main =
  let deps_by_file = Hashtbl.create (List.length dependencies) in
  List.iter dependencies ~f:(fun (file, deps) -> Hashtbl.add deps_by_file file deps);
  let seen = ref String.Set.empty in
  let res = ref [] in
  let rec loop file =
    if not (String.Set.mem file !seen)
    then (
      seen := String.Set.add file !seen;
      List.iter (Hashtbl.find deps_by_file file) ~f:loop;
      res := file :: !res)
  in
  loop (Filename.basename main);
  List.rev !res
;;

let common_build_args name ~external_includes ~external_libraries =
  List.concat
    [ [ "-o"; name ^ ".exe"; "-g" ]
    ; (match Config.mode with
       | Byte -> [ Config.output_complete_obj_arg ]
       | Native -> [])
    ; external_includes
    ; external_libraries
    ]
;;

let allow_unstable_sources = [ "-alert"; "-unstable" ]

let ocaml_warnings =
  let warnings =
    [ (* Warning 49 [no-cmi-file]: no cmi file was found in path for module *)
      "-49"
    ; (* Warning 23: all the fields are explicitly listed in this record: the
        'with' clause is useless. 

         In order to stay version independent, we use a trick with `with` by
         creating a dummy value and filling in the fields available in every
         OCaml version. forced_major_collections is the one missing in versions
         older than 4.12. We therefore disable warning 23 for our purposes. *)
      "-23"
    ; (* Warning 53 [misplaced-attribute]: the "alert" attribute cannot appear
        in this context

        Any .mli files that begin wtih [@@@alert] will cause the compiler to
        emit warning 53 due to the way we alias modules with an `open!`. It may
        be possible to use the command line flag `-open` instead, however this
        complicates dependency tracking so disabling this warning instead is
        suitable for our purposes. *)
      "-53"
    ]
  in
  [ "-w"; String.concat ~sep:"" warnings ]
;;

let build
      ~ocaml_config
      ~dependencies
      ~c_files
      ~asm_files
      ~build_flags
      ~link_flags
      { target = name, main; external_libraries; _ }
  =
  let c_compiler = String.Map.find "c_compiler" ocaml_config in
  let ext_obj =
    try String.Map.find "ext_obj" ocaml_config with
    | Not_found -> ".o"
  in
  let num_dependencies = List.length dependencies in
  let table = Hashtbl.create num_dependencies in
  Status_line.num_jobs := num_dependencies;
  let build m =
    match Hashtbl.find table m with
    | Not_started f ->
      Hashtbl.replace table m Initializing;
      Fiber.fork f
      >>= fun fut ->
      Hashtbl.replace table m (Started fut);
      Fiber.Future.wait fut >>| fun () -> incr Status_line.num_jobs_finished
    | Initializing -> fatal "dependency cycle!"
    | Started fut -> Fiber.Future.wait fut
    | exception Not_found -> fatal "file not found: %s" m
  in
  let external_libraries, external_includes = resolve_externals external_libraries in
  List.iter dependencies ~f:(fun (file, deps) ->
    Hashtbl.add
      table
      file
      (Not_started
         (fun () ->
           Fiber.parallel_iter deps ~f:build
           >>= fun () ->
           Process.run
             ~cwd:build_dir
             Config.compiler
             (List.concat
                [ [ "-c"; "-g"; "-no-alias-deps" ]
                ; ocaml_warnings
                ; allow_unstable_sources
                ; external_includes
                ; [ file ]
                ]))));
  Fiber.fork_and_join_unit
    (fun () -> build (Filename.basename main))
    (fun () ->
       (Fiber.fork_and_join (fun () ->
          Fiber.parallel_map c_files ~f:(fun { Library.name = file; flags } ->
            let flags =
              List.map flags ~f:(fun flag -> [ "-ccopt"; flag ]) |> List.concat
            in
            Process.run
              ~cwd:build_dir
              Config.compiler
              (List.concat
                 [ [ "-c"; "-g" ]; external_includes; build_flags; [ file ]; flags ])
            >>| fun () -> Filename.chop_extension file ^ ext_obj)))
         (fun () ->
            Fiber.parallel_map asm_files ~f:(fun { Library.assembler; flags; out_file } ->
              Process.run
                ~cwd:build_dir
                (match assembler with
                 | `C_comp -> c_compiler
                 | `Msvc_asm -> "ml64.exe")
                flags
              >>| fun () -> out_file))
       >>| fun (x, y) -> x @ y)
  >>= fun obj_files ->
  let compiled_ml_files =
    let compiled_ml_ext =
      match Config.mode with
      | Byte -> ".cmo"
      | Native -> ".cmx"
    in
    List.filter_map (sort_files dependencies ~main) ~f:(fun fn ->
      match Filename.extension fn with
      | ".ml" -> Some (Filename.remove_extension fn ^ compiled_ml_ext)
      | _ -> None)
  in
  write_args "compiled_ml_files" compiled_ml_files;
  Process.run
    ~cwd:build_dir
    Config.compiler
    (let static_flags = if static then [ "-ccopt"; "-static" ] else [] in
     List.concat
       [ common_build_args name ~external_includes ~external_libraries
       ; obj_files
       ; [ "-args"; "compiled_ml_files" ]
       ; link_flags
       ; static_flags
       ; allow_unstable_sources
       ])
;;

let rec rm_rf fn =
  match Unix.lstat fn with
  | { st_kind = S_DIR; _ } ->
    clear fn;
    Unix.rmdir fn
  | _ -> Unix.unlink fn
  | exception Unix.Unix_error (ENOENT, _, _) -> ()

and clear dir = List.iter (readdir dir) ~f:rm_rf

let rec get_flags system = function
  | (set, f) :: r -> if List.mem system ~set then f else get_flags system r
  | [] -> []
;;

(** {2 Bootstrap process} *)
let main () =
  (try clear build_dir with
   | Sys_error _ -> ());
  (try Unix.mkdir build_dir 0o777 with
   | Unix.Unix_error (Unix.EEXIST, _, _) -> ());
  Config.ocaml_config ()
  >>= fun ocaml_config ->
  let word_size = String.Map.find "word_size" ocaml_config in
  let os_type = String.Map.find "os_type" ocaml_config in
  assemble_libraries ~ocaml_config ~word_size ~os_type task
  >>= fun libraries ->
  let c_files =
    List.map ~f:(fun (lib : Library.t) -> lib.c_files) libraries |> List.concat
  in
  let asm_files =
    List.map ~f:(fun (lib : Library.t) -> lib.asm_files) libraries |> List.concat
  in
  get_dependencies libraries
  >>= fun dependencies ->
  let ocaml_system =
    match String.Map.find_opt "system" ocaml_config with
    | None -> assert false
    | Some s -> s
  in
  let build_flags = get_flags ocaml_system Libs.build_flags in
  let link_flags = get_flags ocaml_system Libs.link_flags in
  build ~ocaml_config ~dependencies ~asm_files ~c_files ~build_flags ~link_flags task
;;

let () = Fiber.run (main ())
