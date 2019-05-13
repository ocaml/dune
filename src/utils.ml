open! Stdune
open Import

let system_shell_exn =
  let cmd, arg, os =
    if Sys.win32 then
      ("cmd", "/c", "on Windows")
    else
      ("sh", "-c", "")
  in
  let bin = lazy (Bin.which ~path:(Env.path Env.initial) cmd) in
  fun ~needed_to ->
    match Lazy.force bin with
    | Some path -> (path, arg)
    | None ->
      die "I need %s to %s but I couldn't find it :(\n\
           Who doesn't have %s%s?!"
        cmd needed_to cmd os

let bash_exn =
  let bin = lazy (Bin.which ~path:(Env.path Env.initial) "bash") in
  fun ~needed_to ->
    match Lazy.force bin with
    | Some path -> path
    | None ->
      die "I need bash to %s but I couldn't find it :("
        needed_to

let signal_name =
  let table =
    let open Sys in
    [ sigabrt   , "ABRT"
    ; sigalrm   , "ALRM"
    ; sigfpe    , "FPE"
    ; sighup    , "HUP"
    ; sigill    , "ILL"
    ; sigint    , "INT"
    ; sigkill   , "KILL"
    ; sigpipe   , "PIPE"
    ; sigquit   , "QUIT"
    ; sigsegv   , "SEGV"
    ; sigterm   , "TERM"
    ; sigusr1   , "USR1"
    ; sigusr2   , "USR2"
    ; sigchld   , "CHLD"
    ; sigcont   , "CONT"
    ; sigstop   , "STOP"
    ; sigtstp   , "TSTP"
    ; sigttin   , "TTIN"
    ; sigttou   , "TTOU"
    ; sigvtalrm , "VTALRM"
    ; sigprof   , "PROF"
    (* These ones are only available in OCaml >= 4.03 *)
    ; -22       , "BUS"
    ; -23       , "POLL"
    ; -24       , "SYS"
    ; -25       , "TRAP"
    ; -26       , "URG"
    ; -27       , "XCPU"
    ; -28       , "XFSZ"
    ]
  in
  fun n ->
    match List.assoc table n with
    | None -> sprintf "%d\n" n
    | Some s -> s

type target_kind =
  | Regular of string * Path.Source.t
  | Alias   of string * Path.Source.t
  | Install of string * Path.Source.t
  | Other of Path.t

let analyse_target (fn as original_fn) =
  match Path.extract_build_dir_first_component fn with
  | Some (".aliases", sub) ->
    (match Path.Relative.split_first_component sub with
     | None -> Other fn
     | Some (ctx, fn) ->
       if Path.Relative.is_root fn then
         Other original_fn
       else
         let basename =
           match String.rsplit2 (Path.Relative.basename fn) ~on:'-' with
           | None -> assert false
           | Some (name, digest) ->
             assert (String.length digest = 32);
             name
         in
         Alias (ctx,
                Path.Source.relative
                  (Path.Source.of_relative (Path.Relative.parent_exn fn))
                  basename))
  | Some ("install", sub) ->
    (match Path.Relative.split_first_component sub with
     | None -> Other fn
     | Some (ctx, fn) ->
       Install (ctx, Path.Source.of_relative fn))
  | Some (ctx, sub) ->
    Regular (ctx, Path.Source.of_relative sub)
  | None ->
    Other fn

let describe_target fn =
  let ctx_suffix = function
    | "default" -> ""
    | ctx -> sprintf " (context %s)" ctx
  in
  match analyse_target fn with
  | Alias (ctx, p) ->
    sprintf "alias %s%s" (Path.Source.to_string_maybe_quoted p) (ctx_suffix ctx)
  | Install (ctx, p) ->
    sprintf "install %s%s" (Path.Source.to_string_maybe_quoted p) (ctx_suffix ctx)
  | Regular (ctx, fn) ->
    sprintf "%s%s" (Path.Source.to_string_maybe_quoted fn) (ctx_suffix ctx)
  | Other fn ->
    Path.to_string_maybe_quoted fn

let library_object_directory ~dir name =
  Path.relative dir ("." ^ Lib_name.Local.to_string name ^ ".objs")

let library_native_dir ~obj_dir =
  Path.relative obj_dir "native"

let library_byte_dir ~obj_dir =
  Path.relative obj_dir "byte"

let library_public_cmi_dir ~obj_dir =
  Path.relative obj_dir "public_cmi"

let library_private_dir ~obj_dir =
  Path.relative obj_dir "private"

(* Use "eobjs" rather than "objs" to avoid a potential conflict with a
   library of the same name *)
let executable_object_directory ~dir name =
  Path.relative dir ("." ^ name ^ ".eobjs")

let program_not_found ?context ?hint ~loc prog =
  Errors.fail_opt loc
    "@{<error>Error@}: Program %s not found in the tree or in PATH%s%a"
    (String.maybe_quoted prog)
    (match context with
     | None -> ""
     | Some name -> sprintf " (context: %s)" name)
    (fun fmt -> function
       | None -> ()
       | Some h -> Format.fprintf fmt "@ Hint: %s" h)
    hint

let library_not_found ?context ?hint lib =
  die "@{<error>Error@}: Library %s not found%s%a" (String.maybe_quoted lib)
    (match context with
     | None -> ""
     | Some name -> sprintf " (context: %s)" name)
    (fun fmt -> function
       | None -> ()
       | Some h -> Format.fprintf fmt "@ Hint: %s" h)
    hint

let install_file ~(package : Package.Name.t) ~findlib_toolchain =
  let package = Package.Name.to_string package in
  match findlib_toolchain with
  | None -> package ^ ".install"
  | Some x -> sprintf "%s-%s.install" package x

let line_directive ~filename:fn ~line_number =
  let directive =
    match Filename.extension fn with
    | ".c" | ".cpp" | ".h" -> "line"
    | _ -> ""
  in
  sprintf "#%s %d %S\n" directive line_number fn

let local_bin p = Path.Build.relative p ".bin"

module type Persistent_desc = sig
  type t
  val name : string
  val version : int
end

module Persistent(D : Persistent_desc) = struct
  let magic = sprintf "DUNE-%sv%d:" D.name D.version

  let to_out_string (v : D.t) =
    magic ^ Marshal.to_string v []

  let dump file (v : D.t) =
    Io.with_file_out file ~f:(fun oc ->
      output_string oc magic;
      Marshal.to_channel oc v [])

  let load file =
    if Path.exists file then
      Io.with_file_in file ~f:(fun ic ->
        match really_input_string ic (String.length magic) with
        | exception End_of_file -> None
        | s ->
          if s = magic then
            Some (Marshal.from_channel ic : D.t)
          else
            None)
    else
      None
end

module Cached_digest = struct
  type file =
    { mutable digest            : Digest.t
    ; mutable timestamp         : float
    ; mutable permissions       : Unix.file_perm
    ; mutable stats_checked : int
    }

  type t =
    { mutable checked_key : int
    ; table               : (Path.t, file) Hashtbl.t
    }

  let db_file = Path.relative Path.build_dir ".digest-db"

  module P = Persistent(struct
      type nonrec t = t
      let name = "DIGEST-DB"
      let version = 1
    end)

  let needs_dumping = ref false

  let cache = lazy (
    match P.load db_file with
    | None ->
      { checked_key = 0
      ; table = Hashtbl.create 1024
      }
    | Some cache ->
      cache.checked_key <- cache.checked_key + 1;
      cache)

  let dump () =
    if !needs_dumping && Path.build_dir_exists () then begin
      needs_dumping := false;
      P.dump db_file (Lazy.force cache)
    end

  let () = Hooks.End_of_build.always dump

  let invalidate_cached_timestamps () =
    if Lazy.is_val cache then begin
      let cache = Lazy.force cache in
      cache.checked_key <- cache.checked_key + 1
    end

  let dir_digest (stat : Unix.stats) =
    Marshal.to_string
      ( stat.st_size
      , stat.st_perm
      , stat.st_mtime
      , stat.st_ctime
      ) []
    |> Digest.string

  let path_stat_digest fn stat =
    if stat.Unix.st_kind = Unix.S_DIR then
      dir_digest stat
    else
      Marshal.to_string (Digest.file fn, stat.st_perm) []
      |> Digest.string

  let refresh fn =
    let cache = Lazy.force cache in
    let stat = Path.stat fn in
    let permissions = stat.st_perm in
    let digest = path_stat_digest fn stat in
    needs_dumping := true;
    Hashtbl.replace cache.table ~key:fn
      ~data:{ digest
            ; timestamp = stat.st_mtime
            ; stats_checked = cache.checked_key
            ; permissions
            };
    digest

  let file fn =
    let cache = Lazy.force cache in
    match Hashtbl.find cache.table fn with
    | Some x ->
      if x.stats_checked = cache.checked_key then
        x.digest
      else begin
        needs_dumping := true;
        let stat = Path.stat fn in
        let dirty = ref false in
        if stat.st_mtime <> x.timestamp then begin
          dirty := true;
          x.timestamp <- stat.st_mtime
        end;
        if stat.st_perm <> x.permissions then begin
          dirty := true;
          x.permissions <- stat.st_perm
        end;
        if !dirty then
          x.digest <- path_stat_digest fn stat;
        x.stats_checked <- cache.checked_key;
        x.digest
      end
    | None ->
      refresh fn

  let remove fn =
    let cache = Lazy.force cache in
    needs_dumping := true;
    Hashtbl.remove cache.table fn
end
