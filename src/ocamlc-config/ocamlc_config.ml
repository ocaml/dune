open Stdune
open Result.O

type 'a or_error = ('a, string) Result.t

module String_map = Map.Make(String)

exception E of string
let fail fmt =
  Printf.ksprintf (fun msg -> raise (E msg)) fmt

let errf fmt =
  Printf.ksprintf (fun msg -> Error msg) fmt

let ok_exn = function
  | Ok    x -> x
  | Error e -> raise (E e)

let split_prog ~var s =
  match String.extract_blank_separated_words s with
  | []           -> errf "Variable %S contains only spaces." var
  | prog :: args -> Ok (prog, args)

module Vars = struct
  type t = string String_map.t

  let get_opt t var = String_map.find t var

  let get t var =
    match get_opt t var with
    | Some s -> Ok s
    | None   -> errf "Variable %S not found." var

  let get_bool t var =
    match get_opt t var with
    | None -> Ok false
    | Some s ->
      match s with
      | "true"  -> Ok true
      | "false" -> Ok false
      | _       -> fail "Variable %S is neither 'true' neither 'false'." var

  let get_strings t var =
    match get_opt t var with
    | None   -> []
    | Some s -> String.extract_blank_separated_words s

  let get_prog_and_args t var =
    get t var >>= split_prog ~var
end

type t =
  { vars            : Vars.t
  ; c_compiler      : string      or_error
  ; ocamlc_cflags   : string list or_error
  ; ocamlopt_cflags : string list or_error
  }

let bindings t = t.vars

let sexp_of_t t =
  Usexp.List
    (String_map.to_list t.vars
     |> List.map ~f:(fun (k, v) ->
       Usexp.List [ Usexp.atom_or_quoted_string k
                  ; Usexp.atom_or_quoted_string v
                  ]))

let of_lines lines =
  List.rev_map lines ~f:(fun line ->
    match String.index line ':' with
    | Some i ->
      (String.sub line ~pos:0 ~len:i,
       String.sub line ~pos:(i + 2) ~len:(String.length line - i - 2))
    | None ->
      fail "Unrecognized line: %S" line)
  |> String_map.of_list
  |> function
  | Error (var, _, _) -> fail "Variable %S present twice." var
  | Ok vars ->
    let c_compiler, ocamlc_cflags, ocamlopt_cflags =
      match Vars.get_opt vars "c_compiler" with
      | Some c_compiler -> (* >= 4.06 *)
        let x = split_prog c_compiler ~var:"c_compiler" in
        let get_flags var =
          x >>| fun (_, args) ->
          args @ Vars.get_strings vars var
        in
        (x >>| fst,
         get_flags "ocamlc_flags",
         get_flags "ocamlopt_flags")
      | None ->
        let byte = Vars.get_prog_and_args vars "bytecomp_c_compiler" in
        (byte >>| fst,
         byte >>| snd,
         Vars.get_prog_and_args vars "native_c_compiler" >>| snd)
    in
    { vars
    ; c_compiler
    ; ocamlc_cflags
    ; ocamlopt_cflags
    }

let get_opt     t var = Vars.get_opt t.vars var
let get         t var = ok_exn (Vars.get      t.vars var)
let get_bool    t var = ok_exn (Vars.get_bool t.vars var)
let get_strings t var = Vars.get_strings t.vars var

let c_compiler      t = ok_exn t.c_compiler
let ocamlc_cflags   t = ok_exn t.ocamlc_cflags
let ocamlopt_cflags t = ok_exn t.ocamlopt_cflags

let stdlib_dir t = get t "standard_library"

let natdynlink_supported t =
  Sys.file_exists (Filename.concat (stdlib_dir t) "dynlink.cmxa")

let version_string t = get t "version"

let version t =
  Scanf.sscanf (version_string t) "%u.%u.%u"
    (fun a b c -> a, b, c)

let word_size t = get_opt t "word_size"
let flambda   t = get_bool t "flambda"

let ext_obj t = get t "ext_obj"
let ext_asm t = get t "ext_asm"
let ext_lib t = get t "ext_lib"
let ext_dll t = get t "ext_dll"
let ext_exe t =
  match get t "os_type" with
  | "Win32" -> ".exe"
  | _       -> ""
