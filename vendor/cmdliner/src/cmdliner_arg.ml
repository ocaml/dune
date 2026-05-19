(*---------------------------------------------------------------------------
   Copyright (c) 2011 The cmdliner programmers. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

let rev_compare n0 n1 = compare n1 n0

(* Documentation formatting helpers *)

module Fmt = Cmdliner_base.Fmt

let doc_quote = Cmdliner_base.quote
let doc_alts = Cmdliner_base.alts_str
let doc_alts_enum ?quoted enum = doc_alts ?quoted (List.map fst enum)
let str_of_pp pp v = pp Format.str_formatter v; Format.flush_str_formatter ()

(* Invalid_argument strings *)

let err_not_opt = "Option argument without name"
let err_not_pos = "Positional argument with a name"
let err_incomplete_enum ss =
  Printf.sprintf
    "Arg.enum: missing printable string for a value, other strings are: %s"
    (String.concat ", " ss)

(* Parse error strings *)

let err_no kind s = Fmt.str "no %a %s" Fmt.code_or_quote s kind
let err_not_dir s =
  Fmt.str "%a %a" Fmt.code_or_quote s Fmt.ereason "is not a directory"

let err_is_dir s =
  Fmt.str "%a %a" Fmt.code_or_quote s Fmt.ereason "is a directory"

let err_element kind s exp =
  Fmt.str "%a element in %s (%a): %s"
    Fmt.invalid () kind Fmt.code_or_quote s exp

let err_invalid kind s exp =
  Fmt.str "@[%a %s %a, %s@]" Fmt.invalid () kind Fmt.code_or_quote s exp

let err_invalid_val = err_invalid "value"
let err_sep_miss sep s =
  err_invalid_val s (Fmt.str "%a a '%c' separator" Fmt.missing () sep)

let err_invalid_enum var s enums =
  let pp_docv ppf var =
    if not (var = "ENUM" || var = "") then Fmt.pf ppf "%a " Fmt.code_var var
  in
  Fmt.str "@[%a@ %avalue %a, expected@ %a@]" Fmt.invalid () pp_docv var
    Fmt.code_or_quote s Cmdliner_base.pp_alts enums

(* Argument converters *)

module Completion = Cmdliner_def.Arg_completion
module Conv = Cmdliner_def.Arg_conv
type 'a conv = 'a Conv.t
let some = Cmdliner_def.Arg_conv.some
let some' = Cmdliner_def.Arg_conv.some'
let none = Cmdliner_def.Arg_conv.none

(* Argument information *)

type 'a t = 'a Cmdliner_term.t
type info = Cmdliner_def.Arg_info.t
let info = Cmdliner_def.Arg_info.make

(* Arguments *)

let ( & ) f x = f x
let parse_error e = Error (`Parse e)

let env_bool_parse s = match String.lowercase_ascii s with
| "" | "false" | "no" | "n" | "0" -> Ok false
| "true" | "yes" | "y" | "1" -> Ok true
| s ->
    let alts = doc_alts ~quoted:true ["true"; "yes"; "false"; "no" ] in
    Error (err_invalid_val s alts)

let parse_to_list parser s = match parser s with
| Ok v -> Ok [v] | Error _ as e -> e

let try_env ei a parse ~absent = match Cmdliner_def.Arg_info.env a with
| None -> Ok absent
| Some env ->
    let var = Cmdliner_def.Env.info_var env in
    match Cmdliner_def.Eval.env_var ei var with
    | None -> Ok absent
    | Some v ->
        match parse v with
        | Error e -> parse_error (Cmdliner_msg.err_env_parse env ~err:e)
        | Ok _ as v -> v

let arg_to_args a complete = Cmdliner_def.Arg_info.Set.singleton a complete
let list_to_args f l complete =
  let add acc v = Cmdliner_def.Arg_info.Set.add (f v) complete acc in
  List.fold_left add Cmdliner_def.Arg_info.Set.empty l

let flag a =
  if Cmdliner_def.Arg_info.is_pos a then invalid_arg err_not_opt else
  let convert ei cl = match Cmdliner_def.Cline.get_opt_arg cl a with
  | [] -> try_env ei a env_bool_parse ~absent:false
  | [_, _, None] -> Ok true
  | [_, f, Some v] -> parse_error (Cmdliner_msg.err_flag_value f v)
  | (_, f, _) :: (_ ,g, _) :: _  ->
      parse_error (Cmdliner_msg.err_opt_repeated f g)
  in
  Cmdliner_term.make (arg_to_args a (Conv none)) convert

let flag_all a =
  if Cmdliner_def.Arg_info.is_pos a then invalid_arg err_not_opt else
  let a = Cmdliner_def.Arg_info.make_all_opts a in
  let convert ei cl = match Cmdliner_def.Cline.get_opt_arg cl a with
  | [] -> try_env ei a (parse_to_list env_bool_parse) ~absent:[]
  | l ->
      try
        let truth (_, f, v) = match v with
        | None -> true
        | Some v -> failwith (Cmdliner_msg.err_flag_value f v)
        in
        Ok (List.rev_map truth l)
      with Failure e -> parse_error e
  in
  Cmdliner_term.make (arg_to_args a (Conv none)) convert

let vflag v l =
  let convert _ cl =
    let rec aux fv = function
    | (v, a) :: rest ->
        begin match Cmdliner_def.Cline.get_opt_arg cl a with
        | [] -> aux fv rest
        | [_, f, None] ->
            begin match fv with
            | None -> aux (Some (f, v)) rest
            | Some (g, _) -> failwith (Cmdliner_msg.err_opt_repeated g f)
            end
        | [_, f, Some v] -> failwith (Cmdliner_msg.err_flag_value f v)
        | (_, f, _) :: (_, g, _) :: _ ->
            failwith (Cmdliner_msg.err_opt_repeated g f)
        end
    | [] -> match fv with None -> v | Some (_, v) -> v
    in
    try Ok (aux None l) with Failure e -> parse_error e
  in
  let flag (_, a) =
    if Cmdliner_def.Arg_info.is_pos a then invalid_arg err_not_opt else a
  in
  Cmdliner_term.make (list_to_args flag l (Conv none)) convert

let vflag_all v l =
  let convert _ cl =
    let rec aux acc = function
    | (fv, a) :: rest ->
        begin match Cmdliner_def.Cline.get_opt_arg cl a with
        | [] -> aux acc rest
        | l ->
            let fval (k, f, v) = match v with
            | None -> (k, fv)
            | Some v -> failwith (Cmdliner_msg.err_flag_value f v)
            in
            aux (List.rev_append (List.rev_map fval l) acc) rest
        end
    | [] ->
        if acc = [] then v else List.rev_map snd (List.sort rev_compare acc)
    in
    try Ok (aux [] l) with Failure e -> parse_error e
  in
  let flag (_, a) =
    if Cmdliner_def.Arg_info.is_pos a then invalid_arg err_not_opt else
    Cmdliner_def.Arg_info.make_all_opts a
  in
  Cmdliner_term.make (list_to_args flag l (Conv none)) convert

let parse_opt_value parse f v = match parse v with
| Ok v -> v | Error err -> failwith (Cmdliner_msg.err_opt_parse f ~err)

let opt ?vopt conv v a =
  if Cmdliner_def.Arg_info.is_pos a then invalid_arg err_not_opt else
  let absent = match Cmdliner_def.Arg_info.absent a with
  | Cmdliner_def.Arg_info.Doc d as a when d <> "" -> a
  | _ -> Cmdliner_def.Arg_info.Val (lazy (str_of_pp (Conv.pp conv) v))
  in
  let kind = match vopt with
  | None -> Cmdliner_def.Arg_info.Opt
  | Some dv -> Cmdliner_def.Arg_info.Opt_vopt (str_of_pp (Conv.pp conv) dv)
  in
  let docv = match Cmdliner_def.Arg_info.docv a with
  | "" -> Conv.docv conv | docv -> docv
  in
  let a = Cmdliner_def.Arg_info.make_opt ~docv ~absent ~kind a in
  let convert ei cl = match Cmdliner_def.Cline.get_opt_arg cl a with
  | [] -> try_env ei a (Conv.parser conv) ~absent:v
  | [_, f, Some v] ->
      (try Ok (parse_opt_value (Conv.parser conv) f v) with
      | Failure e -> parse_error e)
  | [_, f, None] ->
      begin match vopt with
      | None -> parse_error (Cmdliner_msg.err_opt_value_missing f)
      | Some optv -> Ok optv
      end
  | (_, f, _) :: (_, g, _) :: _ ->
      parse_error (Cmdliner_msg.err_opt_repeated g f)
  in
  Cmdliner_term.make (arg_to_args a (Conv conv)) convert

let opt_all ?vopt conv v a =
  if Cmdliner_def.Arg_info.is_pos a then invalid_arg err_not_opt else
  let absent = match Cmdliner_def.Arg_info.absent a with
  | Cmdliner_def.Arg_info.Doc d as a when d <> "" -> a
  | _ -> Cmdliner_def.Arg_info.Val (lazy "")
  in
  let kind = match vopt with
  | None -> Cmdliner_def.Arg_info.Opt
  | Some dv -> Cmdliner_def.Arg_info.Opt_vopt (str_of_pp (Conv.pp conv) dv)
  in
  let docv = match Cmdliner_def.Arg_info.docv a with
  | "" -> Conv.docv conv | docv -> docv
  in
  let a = Cmdliner_def.Arg_info.make_opt_all ~docv ~absent ~kind a in
  let convert ei cl = match Cmdliner_def.Cline.get_opt_arg cl a with
  | [] -> try_env ei a (parse_to_list (Conv.parser conv)) ~absent:v
  | l ->
      let parse (k, f, v) = match v with
      | Some v -> (k, parse_opt_value (Conv.parser conv) f v)
      | None -> match vopt with
      | None -> failwith (Cmdliner_msg.err_opt_value_missing f)
      | Some dv -> (k, dv)
      in
      try Ok (List.rev_map snd
                (List.sort rev_compare (List.rev_map parse l))) with
      | Failure e -> parse_error e
  in
  Cmdliner_term.make (arg_to_args a (Conv conv)) convert

(* Positional arguments *)

let parse_pos_value parse a v = match parse v with
| Ok v -> v
| Error err -> failwith (Cmdliner_msg.err_pos_parse a ~err)

let pos ?(rev = false) k conv v a =
  if Cmdliner_def.Arg_info.is_opt a then invalid_arg err_not_pos else
  let absent = match Cmdliner_def.Arg_info.absent a with
  | Cmdliner_def.Arg_info.Doc d as a when d <> "" -> a
  | _ -> Cmdliner_def.Arg_info.Val (lazy (str_of_pp (Conv.pp conv) v))
  in
  let pos = Cmdliner_def.Arg_info.pos ~rev ~start:k ~len:(Some 1) in
  let docv = match Cmdliner_def.Arg_info.docv a with
  | "" -> Conv.docv conv | docv -> docv
  in
  let a = Cmdliner_def.Arg_info.make_pos_abs ~docv ~absent ~pos a in
  let convert ei cl = match Cmdliner_def.Cline.get_pos_arg cl a with
  | [] -> try_env ei a (Conv.parser conv) ~absent:v
  | [v] ->
      (try Ok (parse_pos_value (Conv.parser conv) a v) with
      | Failure e -> parse_error e)
  | _ -> assert false
  in
  Cmdliner_term.make (arg_to_args a (Conv conv)) convert

let pos_list pos conv v a =
  if Cmdliner_def.Arg_info.is_opt a then invalid_arg err_not_pos else
  let docv = match Cmdliner_def.Arg_info.docv a with
  | "" -> Conv.docv conv | docv -> docv
  in
  let a = Cmdliner_def.Arg_info.make_pos ~docv ~pos a in
  let convert ei cl = match Cmdliner_def.Cline.get_pos_arg cl a with
  | [] -> try_env ei a (parse_to_list (Conv.parser conv)) ~absent:v
  | l ->
      try Ok (List.rev (List.rev_map (parse_pos_value (Conv.parser conv) a) l))
      with
      | Failure e -> parse_error e
  in
  Cmdliner_term.make (arg_to_args a (Conv conv)) convert

let all = Cmdliner_def.Arg_info.pos ~rev:false ~start:0 ~len:None
let pos_all c v a = pos_list all c v a

let pos_left ?(rev = false) k =
  let start = if rev then k + 1 else 0 in
  let len = if rev then None else Some k in
  pos_list (Cmdliner_def.Arg_info.pos ~rev ~start ~len)

let pos_right ?(rev = false) k =
  let start = if rev then 0 else k + 1 in
  let len = if rev then Some k else None in
  pos_list (Cmdliner_def.Arg_info.pos ~rev ~start ~len)

(* Arguments as terms *)

let absent_error args =
  let make_req a v acc =
    let req_a = Cmdliner_def.Arg_info.make_req a in
    Cmdliner_def.Arg_info.Set.add req_a v acc
  in
  Cmdliner_def.Arg_info.Set.fold make_req args Cmdliner_def.Arg_info.Set.empty

let value a = a

let err_arg_missing args =
  parse_error @@
  Cmdliner_msg.err_arg_missing (fst (Cmdliner_def.Arg_info.Set.choose args))

let required t =
  let args = absent_error (Cmdliner_term.argset t) in
  let convert ei cl = match (Cmdliner_term.parser t) ei cl with
  | Ok (Some v) -> Ok v
  | Ok None -> err_arg_missing args
  | Error _ as e -> e
  in
  Cmdliner_term.make args convert

let non_empty t =
  let args = absent_error (Cmdliner_term.argset t) in
  let convert ei cl = match (Cmdliner_term.parser t) ei cl with
  | Ok [] -> err_arg_missing args
  | Ok l -> Ok l
  | Error _ as e -> e
  in
  Cmdliner_term.make args convert

let last t =
  let convert ei cl = match (Cmdliner_term.parser t) ei cl with
  | Ok [] -> err_arg_missing (Cmdliner_term.argset t)
  | Ok l -> Ok (List.hd (List.rev l))
  | Error _ as e -> e
  in
  Cmdliner_term.make (Cmdliner_term.argset t) convert

(* Predefined converters. *)

let add_prefix_completion ~token name =
  if Cmdliner_base.string_starts_with ~prefix:token name
  then Some (Completion.string name) else None

let bool =
  let alts = ["true"; "false"] in
  let parser s = try Ok (bool_of_string s) with
  | Invalid_argument _ -> Error (err_invalid_enum "" s alts)
  in
  let completion =
    let func _ctx ~token =
      Ok (List.filter_map (add_prefix_completion ~token) alts)
    in
    Completion.make func
  in
  Conv.make ~docv:"BOOL" ~parser ~pp:Format.pp_print_bool ~completion ()

let char =
  let parser s = match String.length s = 1 with
  | true -> Ok s.[0]
  | false -> Error (err_invalid_val s "expected a character")
  in
  Conv.make ~docv:"CHAR" ~parser ~pp:Fmt.char ()

let parse_with t_of_str exp s =
  try Ok (t_of_str s) with Failure _ -> Error (err_invalid_val s exp)

let int =
  let parser = parse_with int_of_string "expected an integer" in
  Conv.make ~docv:"INT" ~parser ~pp:Format.pp_print_int ()

let int32 =
  let parser = parse_with Int32.of_string "expected a 32-bit integer" in
  let pp ppf = Fmt.pf ppf "%ld" in
  Conv.make ~docv:"INT32" ~parser ~pp ()

let int64 =
  let parser = parse_with Int64.of_string "expected a 64-bit integer" in
  let pp ppf = Fmt.pf ppf "%Ld" in
  Conv.make ~docv:"INT64" ~parser ~pp ()

let nativeint =
  let err = "expected a processor-native integer" in
  let parser = parse_with Nativeint.of_string err in
  let pp ppf = Fmt.pf ppf "%nd" in
  Conv.make ~docv:"NATIVEINT" ~parser ~pp ()

let float =
  let parser = parse_with float_of_string "expected a floating point number" in
  Conv.make ~docv:"DOUBLE" ~parser ~pp:Format.pp_print_float ()

let string = Conv.make ~docv:"" ~parser:Result.ok ~pp:Fmt.string ()

let enum ?(docv = "ENUM") sl =
  if sl = [] then invalid_arg Cmdliner_base.err_empty_list else
  let t = Cmdliner_trie.of_list sl in
  let parser s =
    let legacy_prefixes = Cmdliner_trie.legacy_prefixes ~env:Sys.getenv_opt in
    match Cmdliner_trie.find ~legacy_prefixes t s with
    | Ok _ as v -> v
    | Error `Ambiguous (* Only on legacy prefixes *) ->
        let ambs = List.sort compare (Cmdliner_trie.ambiguities t s) in
        Error (Cmdliner_base.err_ambiguous ~kind:"enum value" s ~ambs)
    | Error `Not_found ->
        let alts = List.rev (List.rev_map (fun (s, _) -> s) sl) in
        Error (err_invalid_enum docv s alts)
  in
  let pp ppf v =
    let sl_inv = List.rev_map (fun (s,v) -> (v,s)) sl in
    try Fmt.string ppf (List.assoc v sl_inv)
    with Not_found -> invalid_arg (err_incomplete_enum (List.map fst sl))
  in
  let completion =
    let func _ctx ~token =
      Ok (List.filter_map (fun (n, _) -> add_prefix_completion ~token n) sl)
    in
    Completion.make func
  in
  Conv.make ~docv ~parser ~pp ~completion ()

let path =
  let parser s = Ok s in
  let pp ppf s = Fmt.string ppf (Filename.quote s) in
  let completion = Completion.complete_paths in
  Conv.make ~docv:"PATH" ~parser ~pp ~completion ()

let filepath =
  let parser s = Ok s in
  let pp ppf s = Fmt.string ppf (Filename.quote s) in
  let completion = Completion.complete_files in
  Conv.make ~docv:"FILE" ~parser ~pp ~completion ()

let dirpath =
  let parser s = Ok s in
  let pp ppf s = Fmt.string ppf (Filename.quote s) in
  let completion = Completion.complete_dirs in
  Conv.make ~docv:"DIR" ~parser ~pp ~completion ()

let file =
  let parser s =
    if s = "-" then Ok s else
    if Sys.file_exists s then Ok s else
    Error (err_no "file or directory" s)
  in
  let completion = Completion.complete_files in
  Conv.make ~docv:"PATH" ~parser ~pp:Fmt.string ~completion ()

let dir =
  let parser s =
    if Sys.file_exists s
    then (if Sys.is_directory s then Ok s else Error (err_not_dir s))
    else Error (err_no "directory" s)
  in
  let completion = Completion.complete_dirs in
  Conv.make ~docv:"DIR" ~parser ~pp:Fmt.string ~completion ()

let non_dir_file =
  let parser s =
    if s = "-" then Ok s else
    if Sys.file_exists s
    then (if not (Sys.is_directory s) then Ok s else Error (err_is_dir s))
    else Error (err_no "file" s)
  in
  let completion = Completion.complete_files in
  Conv.make ~docv:"FILE" ~parser ~pp:Fmt.string ~completion ()

let split_and_parse sep parse s = (* raises [Failure] *)
  let parse sub = match parse sub with
  | Error e -> failwith e | Ok v -> v
  in
  let rec split accum j =
    let i = try String.rindex_from s j sep with Not_found -> -1 in
    if (i = -1) then
      let p = String.sub s 0 (j + 1) in
      if p <> "" then parse p :: accum else accum
    else
    let p = String.sub s (i + 1) (j - i) in
    let accum' = if p <> "" then parse p :: accum else accum in
    split accum' (i - 1)
  in
  split [] (String.length s - 1)

let list ?(sep = ',') conv =
  let parser s = try Ok (split_and_parse sep (Conv.parser conv) s) with
  | Failure e -> Error (err_element "list" s e)
  in
  let rec pp ppf = function
  | [] -> ()
  | v :: l ->
      (Conv.pp conv) ppf v; if (l <> []) then (Fmt.char ppf sep; pp ppf l)
  in
  let docv = Printf.sprintf "%s[%c…]" (Conv.docv conv) sep in
  Conv.make ~docv ~parser ~pp ()

let array ?(sep = ',') conv =
  let parser s =
    try Ok (Array.of_list (split_and_parse sep (Conv.parser conv) s)) with
    | Failure e -> Error (err_element "array" s e)
  in
  let pp ppf v =
    let max = Array.length v - 1 in
    for i = 0 to max do
      Conv.pp conv ppf v.(i); if i <> max then Fmt.char ppf sep
    done
  in
  let docv = Printf.sprintf "%s[%c…]" (Conv.docv conv) sep in
  Conv.make ~docv ~parser ~pp ()

let split_left sep s =
  try
    let i = String.index s sep in
    let len = String.length s in
    Some ((String.sub s 0 i), (String.sub s (i + 1) (len - i - 1)))
  with Not_found -> None

let pair ?(sep = ',') conv0 conv1 =
  let parser s = match split_left sep s with
  | None -> Error (err_sep_miss sep s)
  | Some (v0, v1) ->
      match (Conv.parser conv0) v0, (Conv.parser conv1) v1 with
      | Ok v0, Ok v1 -> Ok (v0, v1)
      | Error e, _ | _, Error e -> Error (err_element "pair" s e)
  in
  let pp ppf (v0, v1) =
    Fmt.pf ppf "%a%c%a" (Conv.pp conv0) v0 sep (Conv.pp conv1) v1
  in
  let docv = Printf.sprintf "%s%c%s" (Conv.docv conv0) sep (Conv.docv conv1) in
  Conv.make ~docv ~parser ~pp ()

let t2 = pair
let t3 ?(sep = ',') conv0 conv1 conv2 =
  let parser s = match split_left sep s with
  | None -> Error (err_sep_miss sep s)
  | Some (v0, s) ->
      match split_left sep s with
      | None -> Error (err_sep_miss sep s)
      | Some (v1, v2) ->
          match (Conv.parser conv0) v0, (Conv.parser conv1) v1,
                (Conv.parser conv2) v2 with
          | Ok v0, Ok v1, Ok v2 -> Ok (v0, v1, v2)
          | Error e, _, _ | _, Error e, _ | _, _, Error e ->
              Error (err_element "triple" s e)
  in
  let pp ppf (v0, v1, v2) =
    let pp = Conv.pp in
    Fmt.pf ppf "%a%c%a%c%a" (pp conv0) v0 sep (pp conv1) v1 sep (pp conv2) v2
  in
  let docv =
    let docv = Conv.docv in
    Printf.sprintf "%s%c%s%c%s" (docv conv0) sep (docv conv1) sep (docv conv2)
  in
  Conv.make ~docv ~parser ~pp ()

let t4 ?(sep = ',') conv0 conv1 conv2 conv3 =
  let parser s = match split_left sep s with
  | None -> Error (err_sep_miss sep s)
  | Some(v0, s) ->
      match split_left sep s with
      | None -> Error (err_sep_miss sep s)
      | Some (v1, s) ->
          match split_left sep s with
          | None -> Error (err_sep_miss sep s)
          | Some (v2, v3) ->
              match (Conv.parser conv0) v0, (Conv.parser conv1) v1,
                    (Conv.parser conv2) v2, (Conv.parser conv3) v3  with
              | Ok v1, Ok v2, Ok v3, Ok v4 -> Ok (v1, v2, v3, v4)
              | Error e, _, _, _ | _, Error e, _, _ | _, _, Error e, _
              | _, _, _, Error e -> Error (err_element "quadruple" s e)
  in
  let pp ppf (v0, v1, v2, v3) =
    let pp = Conv.pp in
    Fmt.pf ppf "%a%c%a%c%a%c%a" (pp conv0) v0 sep (pp conv1) v1 sep (pp conv2)
      v2 sep (pp conv3) v3
  in
  let docv =
    let docv = Conv.docv in
    Printf.sprintf "%s%c%s%c%s%c%s"
      (docv conv0) sep (docv conv1) sep (docv conv2) sep (docv conv3)
  in
  Conv.make ~docv ~parser ~pp ()

(* Predefined arguments *)

let man_fmts =
  ["auto", `Auto; "pager", `Pager; "groff", `Groff; "plain", `Plain]

let man_fmt_docv = "FMT"
let man_fmts_enum = enum ~docv:man_fmt_docv man_fmts
let man_fmts_alts = doc_alts_enum man_fmts
let man_fmts_doc kind =
  Printf.sprintf
    "Show %s in format $(docv). The value $(docv) must be %s. \
     With $(b,auto), the format is $(b,pager) or $(b,plain) whenever \
     the $(b,TERM) env var is $(b,dumb) or undefined."
    kind man_fmts_alts

let man_format =
  let doc = man_fmts_doc "output" in
  let docv = man_fmt_docv in
  value & opt man_fmts_enum `Pager & info ["man-format"] ~docv ~doc

let stdopt_version ~docs =
  value & flag & info ["version"] ~docs ~doc:"Show version information."

let stdopt_help ~docs =
  let doc = man_fmts_doc "this help" in
  let docv = man_fmt_docv in
  value & opt ~vopt:(Some `Auto) (some man_fmts_enum) None &
  info ["help"] ~docv ~docs ~doc

(* Deprecated *)

type 'a printer = 'a Conv.fmt
let docv_default = "VALUE"
let conv' ?docv (parser, pp) = Conv.make ~docv:docv_default ~parser ~pp ()
let conv ?docv (parser, pp) =
  let parser s = match parser s with
  | Ok _ as v -> v | Error (`Msg e) -> Error e
  in
  Conv.make ~docv:docv_default ~parser ~pp ()

let conv_printer = Conv.pp
let conv_docv = Conv.docv
let conv_parser conv =
  fun s -> match Conv.parser conv s with
  | Ok _ as v -> v | Error e -> Error (`Msg e)

let err_invalid s kind =
  `Msg (Printf.sprintf "invalid value '%s', expected %s" s kind)

let parser_of_kind_of_string ~kind k_of_string =
  fun s -> match k_of_string s with
  | None -> Error (err_invalid s kind)
  | Some v -> Ok v
