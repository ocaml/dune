open Import

let system_shell_exn =
  let cmd, arg, os =
    if Sys.win32 then
      ("cmd", "/c", "on Windows")
    else
      ("sh", "-c", "")
  in
  let bin = lazy (Bin.which cmd) in
  fun ~needed_to ->
    match Lazy.force bin with
    | Some path -> (path, arg)
    | None ->
      die "I need %s to %s but I couldn't find it :(\n\
           Who doesn't have %s%s?!"
        cmd needed_to cmd os

let bash_exn =
  let bin = lazy (Bin.which "bash") in
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
    match List.assoc n table with
    | exception Not_found -> sprintf "%d\n" n
    | s -> s

let jbuild_name_in ~dir =
  match Path.extract_build_context dir with
  | None ->
    Path.to_string_maybe_quoted (Path.relative dir "jbuild")
  | Some (ctx_name, dir) ->
    sprintf "%s (context %s)"
      (Path.to_string_maybe_quoted (Path.relative dir "jbuild"))
      ctx_name

type target_kind =
  | Regular of string * Path.t
  | Alias   of string * Path.t
  | Other of Path.t

let analyse_target fn =
  match Path.extract_build_context fn with
  | Some (".aliases", sub) -> begin
      match Path.split_first_component sub with
      | None -> Other fn
      | Some (ctx, fn) ->
        if Path.is_root fn then
          Other fn
        else
          let basename =
            match String.rsplit2 (Path.basename fn) ~on:'-' with
            | None -> assert false
            | Some (name, digest) ->
              assert (String.length digest = 32);
              name
          in
          Alias (ctx, Path.relative (Path.parent fn) basename)
    end
  | Some (ctx, sub) -> Regular (ctx, sub)
  | None ->
    Other fn

let describe_target fn =
  let ctx_suffix = function
    | "default" -> ""
    | ctx -> sprintf " (context %s)" ctx
  in
  match analyse_target fn with
  | Alias (ctx, p) ->
    sprintf "alias %s%s" (Path.to_string_maybe_quoted p) (ctx_suffix ctx)
  | Regular (ctx, fn) ->
    sprintf "%s%s" (Path.to_string_maybe_quoted fn) (ctx_suffix ctx)
  | Other fn ->
    Path.to_string_maybe_quoted fn

let program_not_found ?context ?hint prog =
  die "@{<error>Error@}: Program %s not found in the tree or in PATH%s%a"
    (maybe_quoted prog)
    (match context with
     | None -> ""
     | Some name -> sprintf " (context: %s)" name)
    (fun fmt -> function
       | None -> ()
       | Some h -> Format.fprintf fmt "@ Hint: %s" h)
    hint

let library_not_found ?context ?hint lib =
  die "@{<error>Error@}: Library %s not found%s%a" (maybe_quoted lib)
    (match context with
     | None -> ""
     | Some name -> sprintf " (context: %s)" name)
    (fun fmt -> function
       | None -> ()
       | Some h -> Format.fprintf fmt "@ Hint: %s" h)
    hint

let g () =
  if !Clflags.g then
    ["-g"]
  else
    []

let obj_name_of_basename fn =
  match String.index fn '.' with
  | None -> fn
  | Some i -> String.sub fn ~pos:0 ~len:i

let install_file ~package ~findlib_toolchain =
  match findlib_toolchain with
  | None -> package ^ ".install"
  | Some x -> sprintf "%s-%s.install" package x

module Cached_digest = struct
  type file =
    { mutable digest            : Digest.t
    ; mutable timestamp         : float
    ; mutable timestamp_checked : bool
    }

  let cache = Hashtbl.create 1024

  let file fn =
    match Hashtbl.find cache fn with
    | Some x ->
      if x.timestamp_checked then
        x.digest
      else begin
        let mtime = (Unix.stat (Path.to_string fn)).st_mtime in
        if mtime <> x.timestamp then begin
          let digest = Digest.file (Path.to_string fn) in
          x.digest    <- digest;
          x.timestamp <- mtime;
        end;
        x.timestamp_checked <- true;
        x.digest
      end
    | None ->
      let digest = Digest.file (Path.to_string fn) in
      Hashtbl.add cache ~key:fn
        ~data:{ digest
              ; timestamp = (Unix.stat (Path.to_string fn)).st_mtime
              ; timestamp_checked = true
              };
      digest

  let remove fn = Hashtbl.remove cache fn

  let db_file = "_build/.digest-db"

  let dump () =
    let module Pmap = Path.Map in
    let sexp =
      Sexp.List (
        Hashtbl.fold cache ~init:Pmap.empty ~f:(fun ~key ~data acc ->
          Pmap.add acc ~key ~data)
        |> Path.Map.bindings
        |> List.map ~f:(fun (path, file) ->
          Sexp.List [ Atom (Path.to_string path)
                    ; Atom (Digest.to_hex file.digest)
                    ; Atom (Int64.to_string (Int64.bits_of_float file.timestamp))
                    ]))
    in
    if Sys.file_exists "_build" then
      Io.write_file db_file (Sexp.to_string sexp)

  let load () =
    if Sys.file_exists db_file then begin
      let sexp = Sexp.load ~fname:db_file ~mode:Single in
      let bindings =
        let open Sexp.Of_sexp in
        list
          (triple
             Path.t
             (fun s -> Digest.from_hex (string s))
             (fun s -> Int64.float_of_bits (Int64.of_string (string s)))
          ) sexp
      in
      List.iter bindings ~f:(fun (path, digest, timestamp) ->
        Hashtbl.add cache ~key:path
          ~data:{ digest
                ; timestamp
                ; timestamp_checked = false
                });
    end
end
