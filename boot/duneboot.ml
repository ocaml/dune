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

open Types

(** {2 Utility functions} *)

open StdLabels
open MoreLabels
open Printf

module String = struct
  include String
  module Set = Set.Make (String)
  module Map = Map.Make (String)

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

  [@@@ocaml.warning "-32"]

  let ends_with t ~suffix = Filename.check_suffix t suffix

  let starts_with t ~prefix =
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

  include String
end

module Ml_kind = struct
  type t =
    [ `Mli
    | `Ml
    | `Mll
    | `Mly
    ]

  let all = [ `Mli, ".mli"; `Ml, ".ml"; `Mll, ".mll"; `Mly, ".mly" ]
  let ext t = List.assoc t all

  let of_ext ext =
    let all = List.map all ~f:(fun (x, y) -> y, x) in
    List.assoc_opt ext all
  ;;
end

module Module_name : sig
  type t

  val equal : t -> t -> bool
  val of_fname : string -> t
  val to_fname : t -> kind:Ml_kind.t -> string
  val of_string : string -> t
  val alias_suffix : t -> t
  val to_string : t -> string
  val mangle : t -> prefix:t -> t

  module Set : Set.S with type elt = t
end = struct
  include String

  let mangle t ~prefix = prefix ^ "__" ^ t
  let to_string s = s

  let of_string s =
    assert (s <> "");
    (match s.[0] with
     | 'A' .. 'Z' -> ()
     | _ -> failwith ("invalid module " ^ s));
    for i = 1 to String.length s - 1 do
      match s.[i] with
      | 'A' .. 'Z' | 'a' .. 'z' | '_' | '0' .. '9' -> ()
      | _ -> failwith ("invalid module " ^ s)
    done;
    s
  ;;

  let to_fname t ~kind = String.uncapitalize_ascii t ^ Ml_kind.ext kind

  let of_fname x =
    (match String.index_opt x '.' with
     | None -> x
     | Some len -> String.sub x ~pos:0 ~len)
    |> String.capitalize_ascii
    |> of_string
  ;;

  let alias_suffix s = s ^ "__"
end

module Option = struct
  include Option

  let iter t ~f = Option.iter f t
  let map t ~f = Option.map f t
  let bind t ~f = Option.bind t f
end

module List = struct
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
    List.(rev l, rev m, rev r)
  ;;

  [@@@ocaml.warning "-32"]

  (* Some list functions are introduced in later OCaml versions. There are also
     improvements to performance in some of these. We introduce fallback
     versions allowing compatability >= 4.08 which will get shadowed when the
     stdlib version is available. *)

  (* Introduced in 4.10 *)
  let rec find_map l ~f =
    match l with
    | [] -> None
    | x :: l ->
      (match f x with
       | None -> find_map l ~f
       | Some _ as x -> x)
  ;;

  (* Introduced in 4.14 *)
  let concat_map l ~f = List.map l ~f |> List.concat

  include List
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
    match Sys.getenv_opt "INSIDE_EMACS" with
    | Some (_ : string) -> true
    | None -> false
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

module Io = struct
  (* Return a sorted list of entries in [path] as [path/entry] *)
  let readdir path = Sys.readdir path |> Array.to_list |> List.sort ~cmp:String.compare

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

  let with_file_out file ~f =
    let oc = open_out_bin file in
    let res =
      try Ok (f oc) with
      | exn -> Error exn
    in
    close_out oc;
    match res with
    | Error e -> raise e
    | Ok s -> s
  ;;

  let do_then_copy ~f a b =
    if Sys.file_exists b then fatal "%s already exists" b;
    let s = read_file a in
    with_file_out b ~f:(fun oc ->
      f oc;
      output_string oc s)
  ;;

  (* copy a file - fails if the file exists *)
  let copy a b = do_then_copy ~f:(fun _ -> ()) a b

  (* copy a file and insert a header - fails if the file exists *)
  let copy_with_header ~header a b =
    do_then_copy ~f:(fun oc -> output_string oc header) a b
  ;;

  (* copy a file and insert a directive - fails if the file exists *)
  let copy_with_directive ~directive a b =
    do_then_copy ~f:(fun oc -> fprintf oc "#%s 1 %S\n" directive a) a b
  ;;

  let rec rm_rf fn =
    match Unix.lstat fn with
    | { st_kind = S_DIR; _ } ->
      clear fn;
      Unix.rmdir fn
    | _ -> Unix.unlink fn
    | exception Unix.Unix_error (ENOENT, _, _) -> ()

  and clear dir =
    List.iter (readdir dir) ~f:(fun fn ->
      let path = Filename.concat dir fn in
      rm_rf path)
  ;;
end

module Bin = struct
  let path =
    match Sys.getenv_opt "PATH" with
    | None -> []
    | Some s -> String.split_on_char s ~sep:(if Sys.win32 then ';' else ':')
  ;;

  let find_prog ~f =
    List.find_map path ~f:(fun dir -> Option.map (f dir) ~f:(fun fn -> dir, fn))
  ;;

  let exe = if Sys.win32 then ".exe" else ""
end

(** {2 Concurrency level} *)

let concurrency =
  let try_run_and_capture_line (prog, args) =
    Bin.find_prog ~f:(fun dir ->
      if Sys.file_exists (dir ^/ prog) then Some prog else None)
    |> Option.bind ~f:(fun (dir, prog) ->
      let ic, oc, ec =
        let path = dir ^/ prog in
        let args = Array.of_list @@ (path :: args) in
        Unix.open_process_args_full path args (Unix.environment ())
      in
      let line =
        match input_line ic with
        | s -> Some s
        | exception End_of_file -> None
      in
      match Unix.close_process_full (ic, oc, ec), line with
      | WEXITED 0, Some s -> Some s
      | _ -> None)
  in
  match concurrency with
  | Some n -> n
  | None ->
    (* If no [-j] was given, try to autodetect the number of processors *)
    (if Sys.win32
     then Sys.getenv_opt "NUMBER_OF_PROCESSORS" |> Option.bind ~f:int_of_string_opt
     else
       [ "nproc", []
       ; "getconf", [ "_NPROCESSORS_ONLN" ]
       ; "getconf", [ "NPROCESSORS_ONLN" ]
       ]
       |> List.find_map ~f:(fun cmd ->
         try_run_and_capture_line cmd
         |> Option.bind ~f:(fun s -> int_of_string_opt (String.trim s))))
    |> Option.value ~default:1
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
    val ( >>| ) : 'a t -> ('a -> 'b) -> 'b t
    val ( let+ ) : 'a t -> ('a -> 'b) -> 'b t
    val ( let* ) : 'a t -> ('a -> 'b t) -> 'b t
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
  type 'a t = ('a -> unit) -> unit

  let return x k = k x

  module O = struct
    let ( >>> ) a b k = a (fun () -> b k)
    let ( >>= ) t f k = t (fun x -> f x k)
    let ( >>| ) t f k = t (fun x -> k (f x))
    let ( let+ ) = ( >>| )
    let ( let* ) = ( >>= )
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
    let* a = fork f in
    let* b = fork g in
    both (Future.wait a) (Future.wait b)
  ;;

  let fork_and_join_unit f g =
    let* a = fork f in
    let* b = fork g in
    Future.wait a >>> Future.wait b
  ;;

  let rec parallel_map l ~f =
    match l with
    | [] -> return []
    | x :: l ->
      let* future = fork (fun () -> f x) in
      let* l = parallel_map l ~f in
      let* x = Future.wait future in
      return (x :: l)
  ;;

  let rec parallel_iter l ~f =
    match l with
    | [] -> return ()
    | x :: l ->
      let* future = fork (fun () -> f x) in
      let* () = parallel_iter l ~f in
      Future.wait future
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
      let s = Io.read_file fn in
      Temp.destroy_file fn;
      s
    ;;

    let initial_cwd = Sys.getcwd ()

    let run_process ?cwd prog args ~split =
      let* () = throttle () in
      let stdout_fn, stdout_fd = open_temp_file () in
      let stderr_fn, stderr_fd =
        if split then open_temp_file () else stdout_fn, stdout_fd
      in
      Option.iter cwd ~f:Sys.chdir;
      let pid =
        Unix.create_process
          prog
          (Array.of_list (prog :: args))
          Unix.stdin
          stdout_fd
          stderr_fd
      in
      Option.iter cwd ~f:(fun _ -> Sys.chdir initial_cwd);
      Unix.close stdout_fd;
      if split then Unix.close stderr_fd;
      let ivar = Ivar.create () in
      Hashtbl.add running ~key:pid ~data:ivar;
      let* (status : Unix.process_status) = Ivar.read ivar in
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

module Libs = struct
  let external_libraries = Libs.external_libraries

  let make_lib lib =
    let root_module =
      Option.map lib.root_module ~f:(fun { name; entries } ->
        let name = Module_name.of_string name in
        let entries = List.map ~f:Module_name.of_string entries in
        { name; entries })
    in
    let main_module_name = Option.map ~f:Module_name.of_string lib.main_module_name in
    let special_builtin_support =
      Option.map ~f:Module_name.of_string lib.special_builtin_support
    in
    { lib with root_module; main_module_name; special_builtin_support }
  ;;

  let local_libraries =
    { path = "vendor/re/src"
    ; main_module_name = Some "Re"
    ; include_subdirs = No
    ; special_builtin_support = None
    ; root_module = None
    }
    :: { path = "vendor/spawn/src"
       ; main_module_name = Some "Spawn"
       ; include_subdirs = No
       ; special_builtin_support = None
       ; root_module = None
       }
    :: Libs.local_libraries
    |> List.map ~f:make_lib
  ;;

  let main = make_lib Libs.main
end

type task =
  { target : string * string
  ; external_libraries : string list
  ; local_libraries : Module_name.t library list
  }

let task =
  { target = "dune", "bin/main.ml"
  ; external_libraries = Libs.external_libraries
  ; local_libraries = Libs.local_libraries
  }
;;

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
    let fn = dir ^/ prog ^ ".opt" ^ Bin.exe in
    if Sys.file_exists fn
    then Some fn
    else (
      let fn = dir ^/ prog ^ Bin.exe in
      if Sys.file_exists fn then Some fn else None)
  ;;

  let find_prog prog = Bin.find_prog ~f:(fun dir -> best_prog dir prog)

  let get_prog dir prog =
    match best_prog dir prog with
    | None -> prog_not_found prog
    | Some fn -> fn
  ;;

  let bin_dir, ocamlc =
    if secondary
    then (
      let s =
        Process.run_and_capture
          "ocamlfind"
          [ "-toolchain"; "secondary"; "query"; "ocaml" ]
        |> Fiber.run
      in
      match String.split_lines s with
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
    >>| String.split_lines
    >>| List.fold_left ~init:String.Map.empty ~f:(fun acc line ->
      match Scanf.sscanf line "%[^:]: %s" (fun k v -> k, v) with
      | key, data -> String.Map.add ~key ~data acc
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
    let s = Io.read_file fn in
    Io.with_file_out fn ~f:(fun oc ->
      output_string oc h;
      output_string oc s)
;;

let copy_lexer ~header src dst =
  let dst = Filename.remove_extension dst ^ ".ml" in
  let+ () = Process.run Config.ocamllex [ "-q"; "-o"; dst; src ] in
  insert_header dst ~header
;;

let copy_parser ~header src dst =
  let dst = Filename.remove_extension dst in
  let+ () = Process.run Config.ocamlyacc [ "-b"; dst; src ] in
  insert_header (dst ^ ".ml") ~header;
  insert_header (dst ^ ".mli") ~header
;;

(** {2 Handling of the dune-build-info library} *)

(** {2 Preparation of library files} *)
module Build_info = struct
  let get_version () =
    match
      match Io.read_lines "dune-project" with
      | exception _ -> None
      | lines ->
        List.find_map lines ~f:(fun line ->
          match Scanf.sscanf line "(version %[^)])" (fun v -> v) with
          | exception _ -> None
          | v -> Some v)
    with
    | Some _ as s -> Fiber.return s
    | None ->
      if not (Sys.file_exists ".git")
      then Fiber.return None
      else
        Process.try_run_and_capture
          "git"
          [ "describe"; "--always"; "--dirty"; "--abbrev=7" ]
        >>| Option.map ~f:String.trim
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
    let+ version = get_version () in
    pr
      "let version = %s\n"
      (match version with
       | None -> "None"
       | Some v -> sprintf "Some %S" v);
    pr "\n";
    let libs =
      List.map task.local_libraries ~f:(fun (lib : _ library) -> lib.path, "version")
      @ List.map task.external_libraries ~f:(fun name ->
        name, {|Some "[distributed with OCaml]"|})
      |> List.sort ~cmp:(fun (a, _) (b, _) -> String.compare a b)
    in
    prlist "statically_linked_libraries" libs ~f:(fun (name, v) -> pr "%S, %s\n" name v)
  ;;
end

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

  type ml =
    { kind : [ `Ml | `Mli | `Mll | `Mly ]
    ; name : Module_name.t
    ; module_path : Module_name.t list
    }

  type t =
    | Header
    | C of c
    | Asm of asm
    | Ml of ml

  let analyse ~include_subdirs module_path dn fn =
    let i =
      try String.index fn '.' with
      | Not_found -> String.length fn
    in
    let module_name = String.sub fn ~pos:0 ~len:i in
    let name = lazy (Module_name.of_fname module_name) in
    let module_path =
      lazy
        (let (lazy name) = name in
         match include_subdirs with
         | Qualified -> List.rev (name :: List.map ~f:Module_name.of_fname module_path)
         | No | Unqualified -> [ name ])
    in
    match String.sub fn ~pos:i ~len:(String.length fn - i) with
    | (".S" | ".asm") as ext ->
      let syntax = if ext = ".S" then `Gas else `Intel in
      let os, arch, assembler =
        let fn = Filename.remove_extension fn in
        let check suffix = String.ends_with fn ~suffix in
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
        let check suffix = String.ends_with fn ~suffix in
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
        else if String.ends_with fn ~suffix:"_neon"
        then Some `Arm64, []
        else None, []
      in
      Some (C { arch; flags })
    | ".h" -> Some Header
    | ".defaults.ml" ->
      let fn' = String.sub fn ~pos:0 ~len:i ^ ".ml" in
      if Sys.file_exists (dn ^/ fn')
      then None
      else
        Some
          (Ml { kind = `Ml; name = Lazy.force name; module_path = Lazy.force module_path })
    | ext ->
      Ml_kind.of_ext ext
      |> Option.map ~f:(fun kind ->
        Ml { kind; name = Lazy.force name; module_path = Lazy.force module_path })
  ;;
end

module Source = struct
  type t =
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
end

let gen_module oc bindings =
  List.iter bindings ~f:(fun (lhs, rhs) ->
    fprintf oc "module %s = %s\n" (Module_name.to_string lhs) (Module_name.to_string rhs))
;;

module Wrapper = struct
  type t =
    { toplevel_module : Module_name.t
    ; alias_module : Module_name.t
    }

  let make ~namespace ~modules =
    Option.bind namespace ~f:(fun namespace ->
      if Module_name.Set.equal modules (Module_name.Set.singleton namespace)
      then None
      else if Module_name.Set.mem namespace modules
      then
        Some
          { toplevel_module = namespace
          ; alias_module = Module_name.alias_suffix namespace
          }
      else Some { toplevel_module = namespace; alias_module = namespace })
  ;;

  let mangle_filename t ({ Source.file; kind } : Source.t) =
    match kind with
    | Asm _ | C _ | Header -> Filename.basename file
    | Ml { kind; name; module_path = _ } ->
      let kind =
        match kind with
        | `Mli -> `Mli
        | _ -> `Ml
      in
      let name =
        match t with
        | None -> name
        | Some t ->
          if Module_name.equal name t.toplevel_module
          then name
          else Module_name.mangle ~prefix:t.toplevel_module name
      in
      Module_name.to_fname name ~kind
  ;;

  let header modules =
    List.map modules ~f:(fun m -> Module_name.to_string m |> sprintf "open! %s\n")
    |> String.concat ~sep:""
  ;;

  let generate_wrapper t modules =
    Option.map t ~f:(fun t ->
      let fn = Module_name.to_fname t.alias_module ~kind:`Ml in
      Io.with_file_out (build_dir ^/ fn) ~f:(fun oc ->
        Module_name.Set.remove t.toplevel_module modules
        |> Module_name.Set.elements
        |> List.map ~f:(fun name ->
          let obj = Module_name.mangle ~prefix:t.toplevel_module name in
          name, obj)
        |> gen_module oc);
      fn)
  ;;
end

module Library = struct
  (* Collect source files *)
  let scan ~dir ~include_subdirs =
    let rec iter_paths dir module_path paths acc =
      List.fold_left paths ~init:acc ~f:(fun acc fn ->
        let path = Filename.concat dir fn in
        if Sys.is_directory path
        then (
          match include_subdirs with
          | No -> acc
          | Unqualified | Qualified -> iter_dir path (fn :: module_path) acc)
        else (
          match File_kind.analyse ~include_subdirs module_path dir fn with
          | None -> acc
          | Some kind -> { Source.file = path; kind } :: acc))
    and iter_dir dir module_path acc =
      let paths = Io.readdir dir in
      iter_paths dir module_path paths acc
    in
    iter_dir dir [] []
  ;;

  let modules files ~build_info_module ~root_module =
    let modules =
      List.fold_left
        files
        ~init:Module_name.Set.empty
        ~f:(fun acc { Source.file = _; kind } ->
          match (kind : File_kind.t) with
          | Asm _ | Header | C _ -> acc
          | Ml { kind = _; name; module_path = _ } -> Module_name.Set.add name acc)
    in
    let modules =
      match build_info_module with
      | None -> modules
      | Some m -> Module_name.Set.add m modules
    in
    match root_module with
    | None -> modules
    | Some { name; entries = _ } -> Module_name.Set.add name modules
  ;;

  type t =
    { ocaml_files : string list
    ; alias_file : string option
    ; c_files : Source.c_file list
    ; asm_files : Source.asm_file list
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

  let gen_build_info_module wrapper m =
    let src =
      let fn = Module_name.to_fname m ~kind:`Ml in
      { Source.file = fn; kind = Ml { kind = `Ml; name = m; module_path = [ m ] } }
    in
    let mangled = Wrapper.mangle_filename wrapper src in
    let oc = Io.open_out (build_dir ^/ mangled) in
    let+ () = Build_info.gen_data_module oc in
    close_out oc;
    src, mangled
  ;;

  let process_source_file wrapper ~header ({ Source.file = fn; kind } as source) =
    let+ mangled =
      let mangled = Wrapper.mangle_filename wrapper source in
      let dst = build_dir ^/ mangled in
      match kind with
      | Asm _ ->
        Io.copy fn dst;
        Fiber.return [ mangled ]
      | Header | C _ ->
        Io.copy_with_directive ~directive:"line" fn dst;
        Fiber.return [ mangled ]
      | Ml { kind = `Ml | `Mli; _ } ->
        Io.copy_with_header ~header fn dst;
        Fiber.return [ mangled ]
      | Ml { kind = `Mll; _ } -> copy_lexer fn dst ~header >>> Fiber.return [ mangled ]
      | Ml { kind = `Mly; _ } ->
        (* CR rgrinberg: what if the parser already has an mli? *)
        copy_parser fn dst ~header >>> Fiber.return [ mangled; mangled ^ "i" ]
    in
    List.map mangled ~f:(fun m -> source, m)
  ;;

  let process
        { path = dir
        ; main_module_name = namespace
        ; include_subdirs
        ; special_builtin_support = build_info_module
        ; root_module
        }
        ~ext_obj
        ~ccomp_type
        ~architecture
        ~word_size
        ~os_type
    =
    let files = scan ~dir ~include_subdirs in
    let modules = modules files ~build_info_module ~root_module in
    let wrapper = Wrapper.make ~namespace ~modules in
    let header =
      Option.map wrapper ~f:(fun (m : Wrapper.t) -> m.alias_module)
      |> Option.to_list
      |> Wrapper.header
    in
    let+ files, build_info_file =
      Fiber.fork_and_join
        (fun () ->
           Fiber.parallel_map files ~f:(process_source_file wrapper ~header)
           >>| List.concat)
        (fun () ->
           match build_info_module with
           | None -> Fiber.return None
           | Some m ->
             let+ src, mangled = gen_build_info_module wrapper m in
             Some (src, mangled))
    in
    let root_module =
      Option.map root_module ~f:(fun { name; entries } ->
        let src =
          let fn = Module_name.to_fname name ~kind:`Ml in
          { Source.file = fn; kind = Ml { kind = `Ml; name; module_path = [ name ] } }
        in
        let mangled = Wrapper.mangle_filename wrapper src in
        Io.with_file_out (build_dir ^/ mangled) ~f:(fun oc ->
          List.map entries ~f:(fun entry -> entry, entry) |> gen_module oc);
        src, mangled)
    in
    let alias_file = Wrapper.generate_wrapper wrapper modules in
    let c_files, ocaml_files, asm_files =
      let files =
        let files =
          match build_info_file with
          | None -> files
          | Some fn -> fn :: files
        in
        match root_module with
        | None -> files
        | Some fn -> fn :: files
      in
      List.partition_map_skip files ~f:(fun (src, fn) ->
        match src.kind with
        | C c ->
          if keep_c c ~architecture
          then (
            let extra_flags =
              if
                String.starts_with ~prefix:"blake3_" fn
                && (String.equal os_type "Cygwin" || String.equal word_size "32")
              then
                [ "-DBLAKE3_NO_SSE2"
                ; "-DBLAKE3_NO_SSE41"
                ; "-DBLAKE3_NO_AVX2"
                ; "-DBLAKE3_NO_AVX512"
                ]
              else []
            in
            `Left { Source.flags = extra_flags @ c.flags; name = fn })
          else `Skip
        | Ml _ -> `Middle fn
        | Header -> `Skip
        | Asm asm ->
          if keep_asm asm ~ccomp_type ~architecture ~os_type
          then (
            let out_file = Filename.chop_extension fn ^ ext_obj in
            `Right
              { Source.flags =
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

module Dep = struct
  type 'dep t =
    { file : string
    ; deps : 'dep list
    }

  let empty file = { file; deps = [] }
end

let ocamldep args =
  Process.run_and_capture Config.ocamldep ("-modules" :: args) ~cwd:build_dir
  >>| String.split_lines
  >>| List.map ~f:(fun line ->
    let colon = String.index line ':' in
    let filename = String.sub line ~pos:0 ~len:colon in
    let modules =
      if colon = String.length line - 1
      then []
      else
        String.sub line ~pos:(colon + 2) ~len:(String.length line - colon - 2)
        |> String.split_on_char ~sep:' '
        |> List.map ~f:Module_name.of_string
    in
    { Dep.file = filename; deps = modules })
  >>| List.sort ~cmp:compare
;;

let mk_flags arg l = List.concat_map l ~f:(fun m -> [ arg; m ])
let ccopt x = [ "-ccopt"; x ]

let convert_dependencies ~all_source_files { Dep.file; deps } =
  let is_mli = Filename.check_suffix file ".mli" in
  let convert_module module_name =
    let ml = Module_name.to_fname module_name ~kind:`Ml in
    let mli = Module_name.to_fname module_name ~kind:`Mli in
    if Filename.chop_extension ml = Filename.chop_extension file
    then (* Self-reference *)
      []
    else if String.Set.mem mli all_source_files
    then
      if (not is_mli) && String.Set.mem ml all_source_files
      then
        (* We need to build the .ml for inlining info *)
        [ mli; ml ]
      else (* .mli files never depend on .ml files *)
        [ mli ]
    else if String.Set.mem ml all_source_files
    then
      (* If there's no .mli, then we must always depend on the .ml *)
      [ ml ]
    else (* This is a module coming from an external library *)
      []
  in
  let deps =
    let deps = List.concat_map ~f:convert_module deps in
    (* .ml depends on .mli, if it exists *)
    if (not is_mli) && String.Set.mem (file ^ "i") all_source_files
    then (file ^ "i") :: deps
    else deps
  in
  { Dep.file; deps }
;;

let write_args file args =
  Io.with_file_out (build_dir ^/ file) ~f:(fun ch ->
    output_string ch (String.concat ~sep:"\n" args));
  [ "-args"; file ]
;;

let get_dependencies libraries =
  let+ deps =
    let alias_files =
      List.fold_left libraries ~init:[] ~f:(fun acc (lib : Library.t) ->
        match lib.alias_file with
        | None -> acc
        | Some fn -> fn :: acc)
    in
    let all_source_files =
      List.concat_map libraries ~f:(fun (lib : Library.t) -> lib.ocaml_files)
    in
    let+ dependencies =
      let args = write_args "source_files" all_source_files in
      ocamldep (mk_flags "-map" alias_files @ args)
    in
    List.rev_append
      ((* Alias files have no dependencies *)
       List.rev_map
         alias_files
         ~f:Dep.empty)
      (let all_source_files =
         List.fold_left
           alias_files
           ~init:(String.Set.of_list all_source_files)
           ~f:(fun acc fn -> String.Set.add fn acc)
       in
       List.rev_map dependencies ~f:(convert_dependencies ~all_source_files))
  in
  if debug
  then (
    eprintf "***** Dependencies *****\n";
    List.iter deps ~f:(fun { Dep.file = fn; deps } ->
      eprintf "%s: %s\n" fn (String.concat deps ~sep:" "));
    eprintf "**********\n");
  deps
;;

let assemble_libraries
      { local_libraries; target = _, main; _ }
      ~ext_obj
      ~ccomp_type
      ~architecture
      ~word_size
      ~os_type
  =
  (* In order to assemble all the sources in one place, the executables
       modules are also put in a namespace *)
  local_libraries
  @ [ (let namespace = Module_name.of_fname (Filename.basename main) in
       { Libs.main with main_module_name = Some namespace })
    ]
  |> Fiber.parallel_map
       ~f:(Library.process ~ext_obj ~ccomp_type ~architecture ~word_size ~os_type)
;;

type status =
  | Not_started of (unit -> unit Fiber.t)
  | Initializing
  | Started of unit Fiber.Future.t

let resolve_externals external_libraries =
  let external_libraries, external_includes =
    let convert = function
      | "threads" -> Some ("threads" ^ Config.ocaml_archive_ext, [ "-I"; "+threads" ])
      | "unix" -> Some ("unix" ^ Config.ocaml_archive_ext, Config.unix_library_flags)
      | "seq" | "re" | "spawn" -> None
      | s -> fatal "unhandled external library %s" s
    in
    List.filter_map ~f:convert external_libraries |> List.split
  in
  let external_includes = List.concat external_includes in
  external_libraries, external_includes
;;

let sort_files dependencies ~main =
  let deps_by_file = Hashtbl.create (List.length dependencies) in
  List.iter dependencies ~f:(fun { Dep.file; deps } ->
    Hashtbl.add deps_by_file ~key:file ~data:deps);
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
      ~ext_obj
      ~c_compiler
      ~dependencies
      ~c_files
      ~asm_files
      ~build_flags
      ~link_flags
      { target = name, main; external_libraries; _ }
  =
  let table =
    let num_dependencies = List.length dependencies in
    Status_line.num_jobs := num_dependencies;
    Hashtbl.create num_dependencies
  in
  let build ~file ~deps m =
    match Hashtbl.find table m with
    | Not_started f ->
      let* fut =
        Hashtbl.replace table ~key:m ~data:Initializing;
        Fiber.fork f
      in
      Hashtbl.replace table ~key:m ~data:(Started fut);
      let+ () = Fiber.Future.wait fut in
      incr Status_line.num_jobs_finished
    | Initializing ->
      let deps = String.concat ~sep:", " deps in
      fatal "dependency cycle compiling %s\ndependencies: %s" file deps
    | Started fut -> Fiber.Future.wait fut
    | exception Not_found -> fatal "file not found: %s" m
  in
  let external_libraries, external_includes = resolve_externals external_libraries in
  List.iter dependencies ~f:(fun { Dep.file; deps } ->
    Hashtbl.add
      table
      ~key:file
      ~data:
        (Not_started
           (fun () ->
             let* () = Fiber.parallel_iter deps ~f:(build ~file ~deps) in
             List.concat
               [ [ "-c"; "-g"; "-no-alias-deps" ]
               ; ocaml_warnings
               ; allow_unstable_sources
               ; external_includes
               ; [ file ]
               ]
             |> Process.run ~cwd:build_dir Config.compiler)));
  let* obj_files =
    Fiber.fork_and_join_unit
      (fun () -> build ~file:main ~deps:[] (Filename.basename main))
      (fun () ->
         (Fiber.fork_and_join (fun () ->
            Fiber.parallel_map c_files ~f:(fun { Source.name = file; flags } ->
              let+ () =
                List.concat
                  [ [ "-c"; "-g" ]
                  ; external_includes
                  ; build_flags
                  ; [ file ]
                  ; List.concat_map flags ~f:ccopt
                  ]
                |> Process.run ~cwd:build_dir Config.compiler
              in
              Filename.chop_extension file ^ ext_obj)))
           (fun () ->
              Fiber.parallel_map
                asm_files
                ~f:(fun { Source.assembler; flags; out_file } ->
                  let+ () =
                    Process.run
                      ~cwd:build_dir
                      (match assembler with
                       | `C_comp -> c_compiler
                       | `Msvc_asm -> "ml64.exe")
                      flags
                  in
                  out_file))
         >>| fun (x, y) -> x @ y)
  in
  let args =
    let compiled_ml_files =
      let compiled_ml_ext =
        match Config.mode with
        | Byte -> ".cmo"
        | Native -> ".cmx"
      in
      sort_files dependencies ~main
      |> List.filter_map ~f:(fun fn ->
        match Filename.extension fn with
        | ".ml" -> Some (Filename.remove_extension fn ^ compiled_ml_ext)
        | _ -> None)
    in
    write_args "compiled_ml_files" compiled_ml_files
  in
  List.concat
    [ common_build_args name ~external_includes ~external_libraries
    ; obj_files
    ; args
    ; link_flags
    ; (if static then ccopt "-static" else [])
    ; allow_unstable_sources
    ]
  |> Process.run ~cwd:build_dir Config.compiler
;;

let get_flags system xs =
  List.find_map xs ~f:(fun (set, f) -> if List.mem system ~set then Some f else None)
  |> Option.value ~default:[]
;;

let windows_system_values = [ "win32"; "win64"; "mingw"; "mingw64" ]

let build_flags =
  [ windows_system_values, List.concat_map ~f:ccopt [ "-D_UNICODE"; "-DUNICODE" ] ]
;;

let link_flags =
  let cclib x = [ "-cclib"; x ] in
  (* additional link flags keyed by the platform *)
  [ ( [ "macosx" ]
    , List.concat_map ~f:cclib [ "-framework CoreFoundation"; "-framework CoreServices" ]
    )
  ; windows_system_values, List.concat_map ~f:cclib [ "-lshell32"; "-lole32"; "-luuid" ]
  ; [ "beos" ], cclib "-lbsd" (* flags for Haiku *)
  ]
;;

(** {2 Bootstrap process} *)
let main () =
  (try Io.clear build_dir with
   | Sys_error _ -> ());
  (try Unix.mkdir build_dir 0o777 with
   | Unix.Unix_error (Unix.EEXIST, _, _) -> ());
  let* ocaml_config = Config.ocaml_config () in
  let ext_obj =
    try String.Map.find "ext_obj" ocaml_config with
    | Not_found -> ".o"
  in
  let* libraries =
    let ccomp_type = String.Map.find "ccomp_type" ocaml_config in
    let word_size = String.Map.find "word_size" ocaml_config in
    let os_type = String.Map.find "os_type" ocaml_config in
    let architecture = String.Map.find "architecture" ocaml_config in
    assemble_libraries task ~ext_obj ~ccomp_type ~architecture ~word_size ~os_type
  in
  let c_files = List.concat_map ~f:(fun (lib : Library.t) -> lib.c_files) libraries in
  let asm_files = List.concat_map ~f:(fun (lib : Library.t) -> lib.asm_files) libraries in
  let* dependencies = get_dependencies libraries in
  let ocaml_system = String.Map.find "system" ocaml_config in
  let build_flags = get_flags ocaml_system build_flags in
  let link_flags = get_flags ocaml_system link_flags in
  let c_compiler = String.Map.find "c_compiler" ocaml_config in
  build
    ~ext_obj
    ~c_compiler
    ~dependencies
    ~asm_files
    ~c_files
    ~build_flags
    ~link_flags
    task
;;

let () = Fiber.run (main ())
