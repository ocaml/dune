(*---------------------------------------------------------------------------
   Copyright (c) 2011 The cmdliner programmers. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

let rev_compare n0 n1 = compare n1 n0
let strf = Printf.sprintf

let order_args a0 a1 =
  match Cmdliner_def.Arg_info.is_opt a0, Cmdliner_def.Arg_info.is_opt a1 with
  | true, true -> (* optional by name *)
      let key names =
        let k = List.hd (List.sort rev_compare names) in
        let k = String.lowercase_ascii k in
        if k.[1] = '-' then String.sub k 1 (String.length k - 1) else k
      in
      compare
        (key @@ Cmdliner_def.Arg_info.opt_names a0)
        (key @@ Cmdliner_def.Arg_info.opt_names a1)
  | false, false -> (* positional by variable *)
      compare
        (String.lowercase_ascii @@ Cmdliner_def.Arg_info.docv a0)
        (String.lowercase_ascii @@ Cmdliner_def.Arg_info.docv a1)
  | true, false -> -1 (* positional first *)
  | false, true -> 1  (* optional after *)

let esc = Cmdliner_manpage.escape

let sorted_items_to_blocks ~boilerplate:b items =
  (* Items are sorted by section and then rev. sorted by appearance.
     We gather them by section in correct order in a `Block and prefix
     them with optional boilerplate *)
  let boilerplate = match b with None -> (fun _ -> None) | Some b -> b in
  let mk_block sec acc = match boilerplate sec with
  | None -> (sec, `Blocks acc)
  | Some b -> (sec, `Blocks (b :: acc))
  in
  let rec loop secs sec acc = function
  | (sec', it) :: its when sec' = sec -> loop secs sec (it :: acc) its
  | (sec', it) :: its -> loop (mk_block sec acc :: secs) sec' [it] its
  | [] -> (mk_block sec acc) :: secs
  in
  match items with
  | [] -> []
  | (sec, it) :: its -> loop [] sec [it] its

(* Command docs *)

let invocation ?(sep = " ") ?(ancestors = []) cmd =
  let names = List.rev_map Cmdliner_def.Cmd_info.name (cmd :: ancestors) in
  esc @@ String.concat sep names

let synopsis_pos_arg a =
  let v = match Cmdliner_def.Arg_info.docv a with "" -> "ARG" | v -> v in
  let v = strf "$(i,%s)" (esc v) in
  let v =
    (if Cmdliner_def.Arg_info.is_req a then strf "%s" else strf "[%s]") v
  in
  match Cmdliner_def.Arg_info.(pos_len @@ pos_kind a) with
  | None -> v ^ "…"
  | Some 1 -> v
  | Some n ->
      let rec loop n acc = if n <= 0 then acc else loop (n - 1) (v :: acc) in
      String.concat " " (loop n [])

let synopsis_opt_arg a n =
  let var = match Cmdliner_def.Arg_info.docv a with "" -> "VAL" | v -> v in
  match Cmdliner_def.Arg_info.opt_kind a with
  | Cmdliner_def.Arg_info.Flag -> strf "$(b,%s)" (esc n)
  | Cmdliner_def.Arg_info.Opt ->
        if String.length n > 2
        then strf "$(b,%s)=$(i,%s)" (esc n) (esc var)
        else strf "$(b,%s) $(i,%s)" (esc n) (esc var)
  | Cmdliner_def.Arg_info.Opt_vopt _ ->
      if String.length n > 2
      then strf "$(b,%s)[=$(i,%s)]" (esc n) (esc var)
      else strf "$(b,%s) [$(i,%s)]" (esc n) (esc var)

let deprecated cmd = match Cmdliner_def.Cmd_info.deprecated cmd with
| None -> "" | Some _ -> "(Deprecated) "

let synopsis ?(show_help = false) ?ancestors cmd =
  let show_help = if show_help then " [$(b,--help)]" else "" in
  match Cmdliner_def.Cmd_info.children cmd with
  | [] ->
      let rev_cli_order (a0, _) (a1, _) =
        Cmdliner_def.Arg_info.rev_pos_cli_order a0 a1
      in
      let args = Cmdliner_def.Cmd_info.args cmd in
      let oargs, pargs =
        Cmdliner_def.Arg_info.(Set.partition (fun a _ -> is_opt a) args)
      in
      let oargs =
        (* Keep only those that are listed in the s_options section and
           that are not [--version] or [--help]. * *)
        let keep a _ =
          let drop_names n = n = "--help" || n = "--version" in
          Cmdliner_def.Arg_info.docs a = Cmdliner_manpage.s_options &&
          not (List.exists drop_names (Cmdliner_def.Arg_info.opt_names a))
        in
        let oargs = Cmdliner_def.Arg_info.Set.(elements (filter keep oargs)) in
        let count = List.length oargs in
        let any_option = "[$(i,OPTION)]…" in
        if count = 0 || count > 3 then any_option else
        let syn a =
          let syn =
            synopsis_opt_arg a (Cmdliner_def.Arg_info.opt_name_sample a)
          in
          if Cmdliner_def.Arg_info.is_req a
          then syn
          else strf "[%s]" syn
        in
        let oargs = List.sort order_args oargs in
        let oargs = String.concat " " (List.map syn oargs) in
        String.concat " " [oargs; any_option]
      in
      let pargs =
        let pargs = Cmdliner_def.Arg_info.Set.elements pargs in
        if pargs = [] then "" else
        let pargs = List.map (fun a -> a, synopsis_pos_arg a) pargs in
        let pargs = List.sort rev_cli_order pargs in
        String.concat " " ("" (* add a space *) :: List.rev_map snd pargs)
      in
      strf "%s$(b,%s)%s %s%s"
        (deprecated cmd) (invocation ?ancestors cmd) show_help oargs pargs
  | _cmds ->
      let subcmd = match Cmdliner_def.Cmd_info.has_args cmd with
      | false -> "$(i,COMMAND)" | true -> "[$(i,COMMAND)]"
      in
      strf "%s$(b,%s)%s %s …" (deprecated cmd) (invocation ?ancestors cmd)
        show_help subcmd

let cmd_doc cmd =
  let depr = match Cmdliner_def.Cmd_info.deprecated cmd with
  | None -> "" | Some msg -> msg ^ " "
  in
  depr ^ Cmdliner_def.Cmd_info.doc cmd

let cmd_docs ei = match Cmdliner_def.(Cmd_info.children (Eval.cmd ei)) with
| [] -> []
| cmds ->
    let add_cmd acc cmd =
      let syn = synopsis cmd in
      (Cmdliner_def.Cmd_info.docs cmd, `I (syn, cmd_doc cmd)) :: acc
    in
    let by_sec_by_rev_name (s0, `I (c0, _)) (s1, `I (c1, _)) =
      let c = compare s0 s1 in
      if c <> 0 then c else compare c1 c0 (* N.B. reverse *)
    in
    let cmds = List.fold_left add_cmd [] cmds in
    let cmds = List.sort by_sec_by_rev_name cmds in
    let cmds = (cmds :> (string * Cmdliner_manpage.block) list) in
    sorted_items_to_blocks ~boilerplate:None cmds

(* Argument docs *)

let arg_man_item_label a =
  let s = match Cmdliner_def.Arg_info.is_pos a with
  | true -> strf "$(i,%s)" (esc @@ Cmdliner_def.Arg_info.docv a)
  | false ->
      let names = List.sort compare (Cmdliner_def.Arg_info.opt_names a) in
      String.concat ", " (List.rev_map (synopsis_opt_arg a) names)
  in
  match Cmdliner_def.Arg_info.deprecated a with
  | None -> s | Some _ -> "(Deprecated) " ^ s

let arg_to_man_item ~errs ~subst ~buf a =
  let subst = Cmdliner_def.Arg_info.doclang_subst ~subst a in
  let or_env ~value a = match Cmdliner_def.Arg_info.env a with
  | None -> ""
  | Some e ->
      let value = if value then " or" else "absent " in
      strf "%s $(b,%s) env" value (esc @@ Cmdliner_def.Env.info_var e)
  in
  let absent = match Cmdliner_def.Arg_info.absent a with
  | Cmdliner_def.Arg_info.Err -> "required"
  | Cmdliner_def.Arg_info.Doc "" -> strf "%s" (or_env ~value:false a)
  | Cmdliner_def.Arg_info.Doc s ->
      let s = Cmdliner_manpage.subst_vars ~errs ~subst buf s in
      strf "absent=%s%s" s (or_env ~value:true a)
  | Cmdliner_def.Arg_info.Val v ->
      match Lazy.force v with
      | "" -> strf "%s" (or_env ~value:false a)
      | v -> strf "absent=$(b,%s)%s" (esc v) (or_env ~value:true a)
  in
  let optvopt = match Cmdliner_def.Arg_info.opt_kind a with
  | Cmdliner_def.Arg_info.Opt_vopt v -> strf "default=$(b,%s)" (esc v)
  | _ -> ""
  in
  let argvdoc = match optvopt, absent with
  | "", "" -> ""
  | s, "" | "", s -> strf " (%s)" s
  | s, s' -> strf " (%s) (%s)" s s'
  in
  let deprecated = match Cmdliner_def.Arg_info.deprecated a with
  | None -> "" | Some msg -> msg ^ " "
  in
  let doc = deprecated ^ Cmdliner_def.Arg_info.doc a in
  let doc = Cmdliner_manpage.subst_vars ~errs ~subst buf doc in
  (Cmdliner_def.Arg_info.docs a, `I (arg_man_item_label a ^ argvdoc, doc))

let arg_docs ~errs ~subst ~buf ei =
  let by_sec_by_arg a0 a1 =
    let c = compare
        (Cmdliner_def.Arg_info.docs a0)
        (Cmdliner_def.Arg_info.docs a1)
    in
    if c <> 0 then c else
    let c =
      match
        Cmdliner_def.Arg_info.deprecated a0,
        Cmdliner_def.Arg_info.deprecated a1
      with
      | None, None | Some _, Some _ -> 0
      | None, Some _ -> -1 | Some _, None -> 1
    in
    if c <> 0 then c else order_args a0 a1
  in
  let keep_arg a _ acc =
    if not Cmdliner_def.Arg_info.(is_pos a && (docv a = "" || doc a = ""))
    then (a :: acc) else acc
  in
  let args = Cmdliner_def.Cmd_info.args @@ Cmdliner_def.Eval.cmd ei in
  let args = Cmdliner_def.Arg_info.Set.fold keep_arg args [] in
  let args = List.sort by_sec_by_arg args in
  let args = List.rev_map (arg_to_man_item ~errs ~subst ~buf) args in
  sorted_items_to_blocks ~boilerplate:None args

(* Exit statuses doc *)

let exit_boilerplate sec = match sec = Cmdliner_manpage.s_exit_status with
| false -> None
| true -> Some (Cmdliner_manpage.s_exit_status_intro)

let exit_docs ~errs ~subst ~buf ~has_sexit ei =
  let by_sec (s0, _) (s1, _) = compare s0 s1 in
  let add_exit_item acc einfo =
    let subst = Cmdliner_def.Exit.doclang_subst ~subst einfo in
    let min, max = Cmdliner_def.Exit.info_codes einfo in
    let doc = Cmdliner_def.Exit.info_doc einfo in
    let label = if min = max then strf "%d" min else strf "%d-%d" min max in
    let item = `I (label, Cmdliner_manpage.subst_vars ~errs ~subst buf doc) in
    (Cmdliner_def.Exit.info_docs einfo, item) :: acc
  in
  let exits = Cmdliner_def.Cmd_info.exits @@ Cmdliner_def.Eval.cmd ei in
  let exits = List.sort Cmdliner_def.Exit.info_order exits in
  let exits = List.fold_left add_exit_item [] exits in
  let exits = List.stable_sort by_sec (* sort by section *) exits in
  let boilerplate = if has_sexit then None else Some exit_boilerplate in
  sorted_items_to_blocks ~boilerplate exits

(* Environment doc *)

let env_boilerplate sec = match sec = Cmdliner_manpage.s_environment with
| false -> None
| true -> Some (Cmdliner_manpage.s_environment_intro)

let env_docs ~errs ~subst ~buf ~has_senv ei =
  let add_env_item ~subst (seen, envs as acc) e =
    if Cmdliner_def.Env.Set.mem e seen then acc else
    let seen = Cmdliner_def.Env.Set.add e seen in
    let var = strf "$(b,%s)" @@ esc (Cmdliner_def.Env.info_var e) in
    let var, deprecated = match Cmdliner_def.Env.info_deprecated e with
    | None -> var, "" | Some msg -> "(Deprecated) " ^ var, msg ^ " " in
    let doc = deprecated ^ Cmdliner_def.Env.info_doc e in
    let doc = Cmdliner_manpage.subst_vars ~errs ~subst buf doc in
    let envs = (Cmdliner_def.Env.info_docs e, `I (var, doc)) :: envs in
    seen, envs
  in
  let add_arg_envs a _ acc =
    let envs = Cmdliner_def.Arg_info.doc_envs a in
    let envs = match Cmdliner_def.Arg_info.env a with
    | None -> envs | Some e -> e :: envs
    in
    let subst = Cmdliner_def.Arg_info.doclang_subst ~subst a in
    List.fold_left (add_env_item ~subst) acc envs
  in
  let add_env acc e =
    let subst = Cmdliner_def.Env.doclang_subst ~subst e in
    add_env_item ~subst acc e
  in
  let by_sec_by_rev_name (s0, `I (v0, _)) (s1, `I (v1, _)) =
    let c = compare s0 s1 in
    if c <> 0 then c else compare v1 v0 (* N.B. reverse *)
  in
  (* Arg envs before term envs is important here: if the same is mentioned
     both in an arg and in a term the substs of the arg are allowed. *)
  let args = Cmdliner_def.Cmd_info.args @@ Cmdliner_def.Eval.cmd ei in
  let tenvs = Cmdliner_def.Cmd_info.envs @@ Cmdliner_def.Eval.cmd ei in
  let init = Cmdliner_def.Env.Set.empty, [] in
  let acc = Cmdliner_def.Arg_info.Set.fold add_arg_envs args init in
  let _, envs = List.fold_left add_env acc tenvs in
  let envs = List.sort by_sec_by_rev_name envs in
  let envs = (envs :> (string * Cmdliner_manpage.block) list) in
  let boilerplate = if has_senv then None else Some env_boilerplate in
  sorted_items_to_blocks ~boilerplate envs

(* xref doc *)

let xref_docs ~errs ei =
  let main = Cmdliner_def.Eval.main ei in
  let to_xref = function
  | `Main -> Cmdliner_def.Cmd_info.name main, 1
  | `Tool tool -> tool, 1
  | `Page (name, sec) -> name, sec
  | `Cmd c ->
      (* N.B. we are handling only the first subcommand level here *)
      let cmds = Cmdliner_def.Cmd_info.children main in
      let mname = Cmdliner_def.Cmd_info.name main in
      let is_cmd cmd = Cmdliner_def.Cmd_info.name cmd = c in
      if List.exists is_cmd cmds then strf "%s-%s" mname c, 1 else
      (Format.fprintf errs "xref %s: no such command name@." c; "doc-err", 0)
  in
  let xref_str (name, sec) = strf "%s(%d)" (esc name) sec in
  let xrefs = Cmdliner_def.Cmd_info.man_xrefs @@ Cmdliner_def.Eval.cmd ei in
  let xrefs = match main == Cmdliner_def.Eval.cmd ei with
  | true -> List.filter (fun x -> x <> `Main) xrefs  (* filter out default *)
  | false -> xrefs
  in
  let xrefs = List.fold_left (fun acc x -> to_xref x :: acc) [] xrefs in
  let xrefs = List.(rev_map xref_str (sort rev_compare xrefs)) in
  if xrefs = [] then [] else
  [Cmdliner_manpage.s_see_also, `P (String.concat ", " xrefs)]

(* Man page construction *)

let ensure_s_name ei sm =
  if Cmdliner_manpage.(smap_has_section sm ~sec:s_name) then sm else
  let cmd = Cmdliner_def.Eval.cmd ei in
  let ancestors = Cmdliner_def.Eval.ancestors ei in
  let tname = (deprecated cmd) ^ invocation ~sep:"-" ~ancestors cmd in
  let tdoc = cmd_doc cmd in
  let tagline = if tdoc = "" then "" else strf " - %s" tdoc in
  let tagline = `P (strf "%s%s" tname tagline) in
  Cmdliner_manpage.(smap_append_block sm ~sec:s_name tagline)

let ensure_s_synopsis ei sm =
  if Cmdliner_manpage.(smap_has_section sm ~sec:s_synopsis) then sm else
  let cmd = Cmdliner_def.Eval.cmd ei in
  let ancestors = Cmdliner_def.Eval.ancestors ei in
  let synopsis = `P (synopsis ~ancestors cmd) in
  Cmdliner_manpage.(smap_append_block sm ~sec:s_synopsis synopsis)

let insert_cmd_man_docs ~errs ei sm =
  let buf = Buffer.create 200 in
  let subst = Cmdliner_def.Eval.doclang_subst ei in
  let ins sm (sec, b) = Cmdliner_manpage.smap_append_block sm ~sec b in
  let has_senv = Cmdliner_manpage.(smap_has_section sm ~sec:s_environment) in
  let has_sexit = Cmdliner_manpage.(smap_has_section sm ~sec:s_exit_status) in
  let sm = List.fold_left ins sm (cmd_docs ei) in
  let sm = List.fold_left ins sm (arg_docs ~errs ~subst ~buf ei) in
  let sm = List.fold_left ins sm (exit_docs ~errs ~subst ~buf ~has_sexit ei)in
  let sm = List.fold_left ins sm (env_docs ~errs ~subst ~buf ~has_senv ei) in
  let sm = List.fold_left ins sm (xref_docs ~errs ei) in
  sm

let text ~errs ei =
  let man = Cmdliner_def.Cmd_info.man @@ Cmdliner_def.Eval.cmd ei in
  let sm = Cmdliner_manpage.smap_of_blocks man in
  let sm = ensure_s_name ei sm in
  let sm = ensure_s_synopsis ei sm in
  let sm = insert_cmd_man_docs ei ~errs sm in
  Cmdliner_manpage.smap_to_blocks sm

let title ei =
  let main = Cmdliner_def.Eval.main ei in
  let exec = String.capitalize_ascii (Cmdliner_def.Cmd_info.name main) in
  let cmd = Cmdliner_def.Eval.cmd ei in
  let ancestors = Cmdliner_def.Eval.ancestors ei in
  let name = String.uppercase_ascii (invocation ~sep:"-" ~ancestors cmd) in
  let center_header = esc @@ strf "%s Manual" exec in
  let left_footer =
    let version = match Cmdliner_def.Cmd_info.version main with
    | None -> "" | Some v -> " " ^ v
    in
    esc @@ strf "%s%s" exec version
  in
  name, 1, "", left_footer, center_header

let man ~errs ei = title ei, text ~errs ei

let pp_man ~env ~errs fmt ppf ei =
  let subst = Cmdliner_def.Eval.doclang_subst ei in
  Cmdliner_manpage.print ~env ~errs ~subst fmt ppf (man ~errs ei)

(* Plain synopsis for usage *)

let styled_usage_synopsis ~errs ei =
  let subst = Cmdliner_def.Eval.doclang_subst ei in
  let cmd = Cmdliner_def.Eval.cmd ei in
  let ancestors = Cmdliner_def.Eval.ancestors ei in
  let synopsis = synopsis ~show_help:true ~ancestors cmd in
  Cmdliner_manpage.doc_to_styled ~errs ~subst synopsis
