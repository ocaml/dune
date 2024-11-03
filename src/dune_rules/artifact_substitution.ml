open Import
open Memo.O

(* Artifact substitutions works as follow: the substitution is encoded as a
   string of the form:

   {v %%DUNE_PLACEHOLDER:<len>:<symbolic-value>%% v}

   Where: - [<len>] is the full length of the encoded string -
   [<symbolic-value>] is an encoding of a [Value.t]

   For instance:

   "%%DUNE_PLACEHOLDER:46:vcs-describe:7:src/foo%%"

   The placeholder is padded with '%' characters to make it of length exactly
   [<len>].

   The [copy] functions recognises such strings and substitute them accordingly.
   The maximum allowed length of such a string is 65536.

   When the placeholder is substituted, the replacement is encoded as follow:

   "=<len>:<replacement><space-padding>"

   For instance:

   "=3:1.0 "

   Replacements that are too long are truncated. *)

type configpath =
  | Sourceroot
  | Stdlib

type t =
  | Vcs_describe of Path.Source.t
  | Location of Section.t * Package.Name.t
  | Configpath of configpath
  | Hardcoded_ocaml_path
  | Repeat of int * string

type hardcoded_ocaml_path =
  | Hardcoded of Path.t list
  | Relocatable of Path.t

type status =
  | Some_substitution
  | No_substitution

let to_dyn = function
  | Vcs_describe p -> Dyn.Variant ("Vcs_describe", [ Path.Source.to_dyn p ])
  | Location (kind, lib_name) ->
    Dyn.Variant ("Location", [ Section.to_dyn kind; Package.Name.to_dyn lib_name ])
  | Configpath d ->
    let v =
      match d with
      | Sourceroot -> "Sourceroot"
      | Stdlib -> "Stdlib"
    in
    Dyn.Variant ("Configpath", [ Dyn.Variant (v, []) ])
  | Hardcoded_ocaml_path -> Dyn.Variant ("Hardcoded_ocaml_path", [])
  | Repeat (n, s) -> Dyn.Variant ("Repeat", [ Int n; String s ])
;;

module Conf = struct
  type t =
    { get_vcs : Path.Source.t -> Vcs.t option Memo.t
    ; get_location : Section.t -> Package.Name.t -> Path.t
    ; get_config_path : configpath -> Path.t option Memo.t
    ; sign_hook : (Path.t -> unit Fiber.t) option Memo.t
    ; hardcoded_ocaml_path : hardcoded_ocaml_path Memo.t
    }

  let get_location t = t.get_location

  let mac_codesign_hook ~codesign path =
    let stdout_to =
      Process.Io.make_stdout
        ~output_on_success:Swallow
        ~output_limit:Execution_parameters.Action_output_limit.default
    in
    let stderr_to =
      Process.Io.make_stderr
        ~output_on_success:Swallow
        ~output_limit:Execution_parameters.Action_output_limit.default
    in
    Process.run
      ~stdout_to
      ~stderr_to
      ~display:Quiet
      Strict
      codesign
      [ "-f"; "-s"; "-"; Path.to_string path ]
  ;;

  let sign_hook_of_context (context : Context.t) =
    let+ config =
      let+ ocaml = Context.ocaml context in
      ocaml.ocaml_config
    in
    match Ocaml_config.system config, Ocaml_config.architecture config with
    | "macosx", "arm64" ->
      let codesign_name = "codesign" in
      (match
         let path = Context.path context in
         Bin.which ~path codesign_name
       with
       | None ->
         Utils.program_not_found
           ~loc:None
           ~hint:"codesign should be part of the macOS installation"
           codesign_name
       | Some codesign -> Some (mac_codesign_hook ~codesign))
    | _ -> None
  ;;

  let of_context (context : Context.t) =
    let get_vcs = Source_tree.nearest_vcs in
    let name = Context.name context in
    let get_location = Install.Paths.get_local_location name in
    let get_config_path = function
      | Sourceroot -> Memo.return @@ Some (Path.source Path.Source.root)
      | Stdlib ->
        let+ ocaml = Context.ocaml context in
        Some ocaml.lib_config.stdlib_dir
    in
    let hardcoded_ocaml_path =
      let install_dir =
        let install_dir = Install.Context.dir ~context:name in
        Path.build (Path.Build.relative install_dir "lib")
      in
      let+ default_ocamlpath = Context.default_ocamlpath context in
      Hardcoded (install_dir :: default_ocamlpath)
    in
    let sign_hook = sign_hook_of_context context in
    { get_vcs; get_location; get_config_path; hardcoded_ocaml_path; sign_hook }
  ;;

  let of_install ~relocatable ~roots ~(context : Context.t) =
    let get_vcs = Source_tree.nearest_vcs in
    let hardcoded_ocaml_path =
      match relocatable with
      | Some prefix -> Memo.return @@ Relocatable prefix
      | None ->
        let+ default_ocamlpath = Context.default_ocamlpath context in
        Hardcoded default_ocamlpath
    in
    let get_location section package =
      let paths = Install.Paths.make ~relative:Path.relative ~package ~roots in
      Install.Paths.get paths section
    in
    let get_config_path = function
      | Sourceroot -> Memo.return None
      | Stdlib ->
        let+ ocaml = Context.ocaml context in
        Some ocaml.lib_config.stdlib_dir
    in
    let sign_hook = sign_hook_of_context context in
    { get_location; get_vcs; get_config_path; hardcoded_ocaml_path; sign_hook }
  ;;

  let dummy =
    { get_vcs = (fun _ -> Memo.return None)
    ; get_location = (fun _ _ -> Path.root)
    ; get_config_path = (fun _ -> Memo.return None)
    ; hardcoded_ocaml_path = Memo.return @@ Hardcoded []
    ; sign_hook = Memo.return None
    }
  ;;

  let run_sign_hook t ~has_subst file =
    match has_subst with
    | No_substitution -> Fiber.return ()
    | Some_substitution ->
      let executable =
        match Path.Untracked.stat file with
        | Error _ -> false
        | Ok { st_perm; _ } -> Path.Permissions.test Path.Permissions.execute st_perm
      in
      if executable
      then
        Memo.run t.sign_hook
        |> Fiber.bind ~f:(function
          | Some hook -> hook file
          | None -> Fiber.return ())
      else Fiber.return ()
  ;;
end

let eval t ~(conf : Conf.t) =
  let relocatable path =
    (* return a relative path to the install directory in case of relocatable
       instead of absolute path *)
    conf.hardcoded_ocaml_path
    >>| function
    | Hardcoded _ -> Path.to_absolute_filename path
    | Relocatable install -> Path.reach path ~from:install
  in
  match t with
  | Repeat (n, s) ->
    Fiber.return (Array.make n s |> Array.to_list |> String.concat ~sep:"")
  | Vcs_describe p ->
    Memo.run
      (conf.get_vcs p
       >>= function
       | None -> Memo.return ""
       | Some vcs ->
         let+ res = Vcs.describe vcs in
         Option.value res ~default:"")
  | Location (name, lib_name) ->
    conf.get_location name lib_name |> relocatable |> Memo.run
  | Configpath d ->
    (let* dir = conf.get_config_path d in
     match dir with
     | None -> Memo.return None
     | Some dir -> relocatable dir >>| Option.some)
    >>| Option.value ~default:""
    |> Memo.run
  | Hardcoded_ocaml_path ->
    conf.hardcoded_ocaml_path
    >>| (function
           | Relocatable _ -> "relocatable"
           | Hardcoded l ->
             let l = List.map l ~f:Path.to_absolute_filename in
             "hardcoded\000" ^ String.concat ~sep:"\000" l)
    |> Memo.run
;;

let encode_replacement ~len ~repl:s =
  let repl = sprintf "=%u:%s" (String.length s) s in
  match Int.compare (String.length repl) len with
  | Lt -> repl ^ String.make (len - String.length repl) ' '
  | Eq -> repl
  | Gt -> String.sub repl ~pos:0 ~len
;;

let prefix = "%%DUNE_PLACEHOLDER:"
let prefix_len = String.length prefix
let max_len = 65536

let encode ?(min_len = 0) t =
  let suffix =
    sprintf
      ":%s%%%%"
      (match t with
       | Vcs_describe p ->
         let s = Path.Source.to_string p in
         sprintf "vcs-describe:%d:%s" (String.length s) s
       | Location (kind, name) ->
         let name = Package.Name.to_string name in
         sprintf "location:%s:%d:%s" (Section.to_string kind) (String.length name) name
       | Configpath Sourceroot -> sprintf "configpath:sourceroot:"
       | Configpath Stdlib -> sprintf "configpath:stdlib:"
       | Hardcoded_ocaml_path -> sprintf "hardcoded_ocaml_path:"
       | Repeat (n, s) -> sprintf "repeat:%d:%d:%s" n (String.length s) s)
  in
  let len =
    let len0 = prefix_len + String.length suffix in
    if len0 + 1 < 10
    then len0 + 1
    else if len0 + 2 < 100
    then len0 + 2
    else if len0 + 3 < 1000
    then len0 + 3
    else if len0 + 4 < 10000
    then len0 + 4
    else len0 + 5
  in
  let len = max min_len len in
  if len > max_len
  then
    Code_error.raise
      "Artifact_substitution.encode: too long"
      [ "t", to_dyn t; "min_len", Int min_len; "len", Int len ];
  let s = sprintf "%s%u%s" prefix len suffix in
  s ^ String.make (len - String.length s) '%'
;;

(* This function is not called very often, so the focus is on readability rather
   than speed. *)
let decode s =
  let fail () = raise_notrace Exit in
  let parse_int s =
    match Int.of_string s with
    | None -> fail ()
    | Some i -> i
  in
  let len = String.length s in
  match
    if len > max_len
       || len < 4
       || s.[0] <> '%'
       || s.[1] <> '%'
       || s.[len - 2] <> '%'
       || s.[len - 1] <> '%'
    then fail ();
    let dune_placeholder, len', rest =
      match String.split (String.sub s ~pos:2 ~len:(len - 4)) ~on:':' with
      | dune_placeholder :: len' :: rest -> dune_placeholder, len', rest
      | _ -> fail ()
    in
    if dune_placeholder <> "DUNE_PLACEHOLDER" then fail ();
    if parse_int len' <> len then fail ();
    let read_string_payload = function
      | [] -> fail ()
      | len :: rest ->
        let len = parse_int len in
        let s = String.concat rest ~sep:":" in
        for i = len to String.length s - 1 do
          if s.[i] <> '%' then fail ()
        done;
        String.sub s ~pos:0 ~len
    in
    match rest with
    | "vcs-describe" :: rest ->
      let path = Path.Source.of_string (read_string_payload rest) in
      Vcs_describe path
    | "location" :: kind :: rest ->
      let name = Package.Name.of_string (read_string_payload rest) in
      let kind = Option.value_exn (Section.of_string kind) in
      Location (kind, name)
    | "configpath" :: "sourceroot" :: _ -> Configpath Sourceroot
    | "configpath" :: "stdlib" :: _ -> Configpath Stdlib
    | "hardcoded_ocaml_path" :: _ -> Hardcoded_ocaml_path
    | "repeat" :: repeat :: rest -> Repeat (parse_int repeat, read_string_payload rest)
    | _ -> fail ()
  with
  | exception Exit -> None
  | t -> Option.some_if (encode t ~min_len:len = s) t
;;

(* Scan a buffer for "%%DUNE_PLACEHOLDER:<len>:" *)
module Scanner = struct
  (* The following module implement a scanner for special placeholders strings.
     The scanner needs to be fast as possible as it will scan every single byte
     of the input so this module is carefully written with performances in mind.

     The logic is implemented as a DFA where each OCaml function represent a
     state of the automaton.

     In the future, we expect to have a C version of the scanner so that it can
     run concurrently with OCaml code in a separate thread. This will speed up
     the promotion of large binaries from the build directory to the source
     tree.

     {1 Notations}

     In this module, [buf] is the buffer containing the data read from the
     input. [end_of_data] is the position in [buf] of the end of data read from
     the input. [pos] is the current reading position in [buf].
     [placeholder_start] represents the position in [buf] where the current
     potential placeholder starts in [buf]. *)

  (* The following type represents the possible state of the DFA. *)
  type state =
    | Scan0
    (* Initial state and state when we are not in a potential placeholder *)
    | Scan1
    (* State after seeing one '%' *)
    | Scan2
    (* State after seeing at least two '%' *)
    | Scan_prefix of int
    (* [Scan_prefix placeholder_start] is the state after seeing [pos -
       placeholder_start] characters from [prefix] *)
    | Scan_length of int * int
    (* [Scan_length (placeholder_start, acc)] is the state after seeing all of
       [prefix] and the beginning of the length field. [acc] is the length
       accumulated so far. *)
    | Scan_placeholder of int * int

  (* [Scan_placeholder (placeholder_start, len)] is the state after seeing all
     of [prefix] and the length field, i.e. just after the second ':' of the
     placeholder *)

  (* The [run] function at the end of this module is the main function that
     consume a buffer and return the new DFA state. If the beginning of
     placeholder is found before reaching the end of the buffer, [run]
     immediately returns [Scan_placeholder (placeholder_start, len)].

     The following functions represent the transition functions for each state
     of the DFA. [run] is called only at the beginning and immediately chain the
     execution to the right transition function.

     Each transition function is written as follow:

     {[
       let state ~buf ~pos ~end_of_data ... = if pos < end_of_data then let c =
       Bytes.unsafe_get buf pos in let pos = pos + 1 in match c with ... else
       State (...)
     ]}

     i.e. it either inspect the next character and chain the execution to the
     next transition function or immediately return the DFA state if the end of
     buffer is reached. Reaching the end of buffer is handled in the [else]
     branch for performance reason: the code generated by OCaml tend to make
     following the [then] branch slightly more efficient and reaching the end of
     buffer is the less likely case. *)

  let rec scan0 ~buf ~pos ~end_of_data =
    if pos < end_of_data
    then (
      let c = Bytes.unsafe_get buf pos in
      let pos = pos + 1 in
      match c with
      | '%' -> scan1 ~buf ~pos ~end_of_data
      | _ -> scan0 ~buf ~pos ~end_of_data)
    else Scan0

  and scan1 ~buf ~pos ~end_of_data =
    if pos < end_of_data
    then (
      let c = Bytes.unsafe_get buf pos in
      let pos = pos + 1 in
      match c with
      | '%' -> scan2 ~buf ~pos ~end_of_data
      | _ -> scan0 ~buf ~pos ~end_of_data)
    else Scan1

  and scan2 ~buf ~pos ~end_of_data =
    if pos < end_of_data
    then (
      let c = Bytes.unsafe_get buf pos in
      let pos = pos + 1 in
      match c with
      | '%' -> scan2 ~buf ~pos ~end_of_data
      | 'D' -> scan_prefix ~buf ~pos ~end_of_data ~placeholder_start:(pos - 3)
      | _ -> scan0 ~buf ~pos ~end_of_data)
    else Scan2

  and scan_prefix ~buf ~pos ~end_of_data ~placeholder_start =
    if pos < end_of_data
    then (
      let pos_in_prefix = pos - placeholder_start in
      let c = Bytes.unsafe_get buf pos in
      let pos = pos + 1 in
      match c with
      | '%' -> scan1 ~buf ~pos ~end_of_data
      | c ->
        if c = prefix.[pos_in_prefix]
        then
          if pos_in_prefix = prefix_len - 1
          then scan_length ~buf ~pos ~end_of_data ~placeholder_start ~acc:0
          else scan_prefix ~buf ~pos ~end_of_data ~placeholder_start
        else scan0 ~buf ~pos ~end_of_data)
    else Scan_prefix placeholder_start

  and scan_length ~buf ~pos ~end_of_data ~placeholder_start ~acc =
    if pos < end_of_data
    then (
      let c = Bytes.unsafe_get buf pos in
      let pos = pos + 1 in
      match c with
      | '%' -> scan1 ~buf ~pos ~end_of_data
      | '0' .. '9' as c ->
        let n = Char.code c - Char.code '0' in
        let acc = (acc * 10) + n in
        if acc = 0 || acc > max_len
        then
          (* We don't allow leading zeros in length fields and a length of [0]
             is not possible, so [acc = 0] here correspond to an invalid
             placeholder *)
          scan0 ~buf ~pos ~end_of_data
        else scan_length ~buf ~pos ~end_of_data ~placeholder_start ~acc
      | ':' ->
        if acc < pos - placeholder_start
        then
          (* If the length is too small, then this is surely not a valid
             placeholder *)
          scan0 ~buf ~pos ~end_of_data
        else Scan_placeholder (placeholder_start, acc)
      | _ -> scan0 ~buf ~pos ~end_of_data)
    else Scan_length (placeholder_start, acc)
  ;;

  let run state ~buf ~pos ~end_of_data =
    match state with
    | Scan0 -> scan0 ~buf ~pos ~end_of_data
    | Scan1 -> scan1 ~buf ~pos ~end_of_data
    | Scan2 -> scan2 ~buf ~pos ~end_of_data
    | Scan_prefix placeholder_start ->
      scan_prefix ~buf ~pos ~end_of_data ~placeholder_start
    | Scan_length (placeholder_start, acc) ->
      scan_length ~buf ~pos ~end_of_data ~placeholder_start ~acc
    | Scan_placeholder _ -> state
  ;;
end

let buf_len = max_len

type mode =
  | Test
  | Copy of
      { input_file : Path.t
      ; output : bytes -> int -> int -> unit
      ; conf : Conf.t
      }

(** The copy algorithm works as follow:

    {v
       read some data from the input
                |
                |
                v
        feed data to [Scanner.run]<-----------------------------------------\
                |                                                           |
                |                                                           |
                v                                                           |
   commit all the data we are sure are not                                  |
     part of a placeholder to the output                                    |
                |                                                           |
                |                                                           |
                v                                                           |
 was the beginning of a placeholder found by [Scanner.run]?                 |
          (i.e. "%%DUNE_PLACEHOLDER:<len>:")                                |
 and if yes, is the whole placeholder currently in [buf]?                   |
        |                                         |                         |
        | YES                                     | NO                      |
        v                                         v                         |
 extract the placeholder                   read more data from the input    |
 and try to parse it with                         |                         |
 [Artifact_substitution.decode]                   \-------------------------|
 |                            |                                             |
 | SUCCESS                    |                                             |
 v                            |                                             |
output the replacement        |                                             |
 |                            | FAILURE                                     |
 |                            v                                             |
 |                    consider that this                                    |
 |                    wasn't a placeholder                                  |
 |                            |                                             |
 |                            \---------------------------------------------|
 |                                                                          |
 \--------------------------------------------------------------------------/
    v} *)
let parse ~input ~mode =
  let open Fiber.O in
  let buf = Bytes.create buf_len in
  let rec loop scanner_state ~beginning_of_data ~pos ~end_of_data ~status =
    let scanner_state = Scanner.run scanner_state ~buf ~pos ~end_of_data in
    let placeholder_start =
      match scanner_state with
      | Scan0 -> end_of_data
      | Scan1 -> end_of_data - 1
      | Scan2 -> end_of_data - 2
      | Scan_prefix placeholder_start
      | Scan_length (placeholder_start, _)
      | Scan_placeholder (placeholder_start, _) -> placeholder_start
    in
    (* All the data before [placeholder_start] can be sent to the output
       immediately since we know for sure that they are not part of a
       placeholder *)
    (match mode with
     | Test -> ()
     | Copy { output; _ } ->
       if placeholder_start > beginning_of_data
       then output buf beginning_of_data (placeholder_start - beginning_of_data));
    let leftover = end_of_data - placeholder_start in
    match scanner_state with
    | Scan_placeholder (placeholder_start, len) when len <= leftover ->
      let placeholder = Bytes.sub_string buf ~pos:placeholder_start ~len in
      (match decode placeholder with
       | Some t ->
         (match mode with
          | Test -> Fiber.return Some_substitution
          | Copy { output; input_file; conf } ->
            let* s = eval t ~conf in
            (if !Clflags.debug_artifact_substitution
             then
               let open Pp.O in
               Console.print
                 [ Pp.textf
                     "Found placeholder in %s:"
                     (Path.to_string_maybe_quoted input_file)
                 ; Pp.enumerate
                     ~f:Fun.id
                     [ Pp.text "placeholder: " ++ Dyn.pp (to_dyn t)
                     ; Pp.text "evaluates to: " ++ Dyn.pp (String s)
                     ]
                 ]);
            let s = encode_replacement ~len ~repl:s in
            output (Bytes.unsafe_of_string s) 0 len;
            let pos = placeholder_start + len in
            loop Scan0 ~beginning_of_data:pos ~pos ~end_of_data ~status:Some_substitution)
       | None ->
         (* Restart just after [prefix] since we know for sure that a placeholder
            cannot start before that. *)
         loop
           Scan0
           ~beginning_of_data:placeholder_start
           ~pos:(placeholder_start + prefix_len)
           ~end_of_data
           ~status)
    | scanner_state ->
      (* We reached the end of the buffer: move the leftover data back to the
         beginning of [buf] and refill the buffer *)
      if leftover > 0
      then
        Bytes.blit ~src:buf ~dst:buf ~src_pos:placeholder_start ~dst_pos:0 ~len:leftover;
      (* Reset [placeholder_start] to [0] since we moved back the leftover data
         to the beginning of [buf] *)
      let scanner_state : Scanner.state =
        match scanner_state with
        | Scan0 | Scan1 | Scan2 -> scanner_state
        | Scan_prefix _ -> Scan_prefix 0
        | Scan_length (_, acc) -> Scan_length (0, acc)
        | Scan_placeholder (_, len) -> Scan_placeholder (0, len)
      in
      (match input buf leftover (buf_len - leftover) with
       | 0 ->
         (match scanner_state with
          | Scan_placeholder _ ->
            (* There might still be another placeholder after this invalid one
               with a length that is too long *)
            loop Scan0 ~beginning_of_data:0 ~pos:prefix_len ~end_of_data:leftover ~status
          | _ ->
            (match mode with
             | Test -> Fiber.return No_substitution
             | Copy { output; _ } ->
               (* Nothing more to read; [leftover] is definitely not the beginning
                  of a placeholder, send it and end the copy *)
               output buf 0 leftover;
               Fiber.return status))
       | n ->
         loop
           scanner_state
           ~beginning_of_data:0
           ~pos:leftover
           ~end_of_data:(leftover + n)
           ~status)
  in
  match input buf 0 buf_len with
  | 0 -> Fiber.return No_substitution
  | n -> loop Scan0 ~beginning_of_data:0 ~pos:0 ~end_of_data:n ~status:No_substitution
;;

let copy ~conf ~input_file ~input ~output =
  parse ~input ~mode:(Copy { conf; input_file; output })
;;

let copy_file_non_atomic ~conf ?chmod ~src ~dst () =
  (* CR-rgrinberg: our copying here is slow. If we scan the file and detect no
     substitutions, we should go directly to [Io.copy_file] *)
  let open Fiber.O in
  let* ic, oc = Fiber.return (Io.setup_copy ?chmod ~src ~dst ()) in
  Fiber.finalize
    ~finally:(fun () ->
      Io.close_both (ic, oc);
      Fiber.return ())
    (fun () -> copy ~conf ~input_file:src ~input:(input ic) ~output:(output oc))
;;

(** This is just an optimisation: skip the renaming if the destination exists
    and has the right digest. The optimisation is useful to avoid unnecessary
    retriggering of Dune and other file-watching systems. *)
let replace_if_different ~delete_dst_if_it_is_a_directory ~src ~dst =
  let up_to_date =
    match Path.Untracked.stat dst with
    | Ok { st_kind = S_DIR; _ } ->
      (match delete_dst_if_it_is_a_directory with
       | true ->
         Path.rm_rf dst;
         false
       | false ->
         User_error.raise
           [ Pp.textf
               "Cannot copy artifact to %S because it is a directory"
               (Path.to_string dst)
           ])
    | Error (_ : Unix_error.Detailed.t) -> false
    | Ok (_ : Unix.stats) ->
      let temp_file_digest = Digest.file src in
      let dst_digest = Digest.file dst in
      Digest.equal temp_file_digest dst_digest
  in
  if not up_to_date then Path.rename src dst
;;

let copy_file ~conf ?chmod ?(delete_dst_if_it_is_a_directory = false) ~src ~dst () =
  (* We create a temporary file in the same directory to ensure it's on the same
     partition as [dst] (otherwise, [Path.rename temp_file dst] won't work). The
     prefix ".#" is used because Dune ignores such files and so creating this
     file will not trigger a rebuild. *)
  let temp_file =
    let dst_dir = Path.parent_exn dst in
    let dst_name = Path.basename dst in
    Path.relative dst_dir (sprintf ".#%s.dune-temp" dst_name)
  in
  Fiber.finalize
    (fun () ->
      let open Fiber.O in
      Path.parent dst |> Option.iter ~f:Path.mkdir_p;
      let* has_subst = copy_file_non_atomic ~conf ?chmod ~src ~dst:temp_file () in
      let+ () = Conf.run_sign_hook conf ~has_subst temp_file in
      replace_if_different ~delete_dst_if_it_is_a_directory ~src:temp_file ~dst)
    ~finally:(fun () ->
      Path.unlink_no_err temp_file;
      Fiber.return ())
;;

let test_file ~src () =
  let open Fiber.O in
  let* ic = Fiber.return (Io.open_in src) in
  Fiber.finalize
    ~finally:(fun () ->
      Io.close_in ic;
      Fiber.return ())
    (fun () -> parse ~input:(input ic) ~mode:Test)
;;
