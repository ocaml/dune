(*---------------------------------------------------------------------------
   Copyright (c) 2011 The cmdliner programmers. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(* A command line stores pre-parsed information about the command
   line's arguments in a more structured way. Given the
   Cmdliner_def.Arg_info.t values mentioned in a term and Sys.argv
   (without exec name) we parse the command line into
   [Cmdliner_def.Cline.t] which is map of [Cmdliner_def.Arg_info.t]
   values to [Cmdliner_def.Cline.arg] values. This map is used by the
   term's closures to retrieve and convert command line arguments (see
   the [Cmdliner_arg] module). *)

(* Completion *)

let complete_prefix = "--__complete="
let has_complete_prefix s =
  Cmdliner_base.string_starts_with ~prefix:complete_prefix s

let get_token_to_complete s =
  Cmdliner_base.string_drop_first (String.length complete_prefix) s

let is_opt_to_complete s = (* assert (has_complete_prefix s) *)
  String.length s > String.length complete_prefix &&
  s.[String.length complete_prefix] = '-'

let maybe_token_to_complete ~for_completion s =
  if not for_completion || not (has_complete_prefix s) then None else
  Some (get_token_to_complete s)

(* Command lines *)

let err_multi_opt_name_def name arg_info arg_info' =
  Cmdliner_base.err_multi_def ~kind:"option name" name
    Cmdliner_def.Arg_info.doc arg_info arg_info'

let arg_info_indexes arg_infos =
  (* from [args] returns a trie mapping the names of optional arguments to
     their arg_info, a list with all arg_info for positional arguments and
     a Cmdliner_def.Cline.t mapping each arg_info to an empty [arg]. *)
  let rec loop optidx posidx cline = function
  | [] -> optidx, posidx, cline
  | arg_info :: l ->
      match Cmdliner_def.Arg_info.is_pos arg_info with
      | true ->
          let cline = Cmdliner_def.Cline.add arg_info (P []) cline in
          loop optidx (arg_info :: posidx) cline l
      | false ->
          let add t name = match Cmdliner_trie.add t name arg_info with
          | `New t -> t
          | `Replaced (a', _) ->
              invalid_arg (err_multi_opt_name_def name arg_info a')
          in
          let names = Cmdliner_def.Arg_info.opt_names arg_info in
          let optidx = List.fold_left add optidx names in
          let cline = Cmdliner_def.Cline.add arg_info (O []) cline in
          loop optidx posidx cline l
  in
  let cline = Cmdliner_def.Cline.empty in
  let arg_infos = Cmdliner_def.Arg_info.Set.elements arg_infos in
  loop Cmdliner_trie.empty [] cline arg_infos

(* Optional argument parsing *)

(* Note on option completion. Technically when trying to complete an
   option we could try to avoid mentioning names that have already be
   mentioned and that are not repeatable. Sometimes not being able to
   complete what we know exists ends up being more confusing than
   enlightening so we don't do that for now.

   Also the code is quite messy, perhaps we should cleanly separate
   parsing for completion and parsing for evaluation. *)

let is_opt s = String.length s > 1 && s.[0] = '-'
let is_short_opt s = String.length s = 2 && s.[0] = '-'

let parse_opt_arg s =
  (* (name, value) of opt arg, assert len > 1. except if complete *)
  let is_completion = has_complete_prefix s in
  let s = if is_completion then get_token_to_complete s else s in
  let l = String.length s in
  if l <= 1 then "-", None, is_completion else
  if s.[1] <> '-' then (* short opt *)
    if l = 2 then s, None, is_completion else
    String.sub s 0 2, Some (String.sub s 2 (l - 2)) (* with glued opt arg *),
    is_completion
  else try (* long opt *)
    let i = String.index s '=' in
    String.sub s 0 i, Some (String.sub s (i + 1) (l - i - 1)), is_completion
  with Not_found -> s, None, is_completion

let hint_matching_opt optidx s =
  (* hint option names that could match [s] in [optidx]. *)
  if String.length s <= 2 then [] else
  let short_opt, long_opt =
    if s.[1] <> '-'
    then s, Printf.sprintf "-%s" s
    else String.sub s 1 (String.length s - 1), s
  in
  let short_opt, _, _ = parse_opt_arg short_opt in
  let long_opt, _, _ = parse_opt_arg long_opt in
  let all = Cmdliner_trie.ambiguities optidx "-" in
  match List.mem short_opt all, Cmdliner_base.suggest long_opt all with
  | false, [] -> []
  | false, l -> l
  | true, [] -> [short_opt]
  | true, l -> if List.mem short_opt l then l else short_opt :: l

let parse_opt_value ~for_completion cline arg_info name value args =
  (* Either we got a value glued in [value] or we need to get one in [args]
     in this case we need to take care of a possible completion token *)
  match Cmdliner_def.Arg_info.opt_kind arg_info with
  | Flag -> (* Flags have no values but we may get dash sharing in [value] *)
      begin match value with
      | None -> None, None, args
      | Some v when is_short_opt name -> (* short flag dash sharing *)
          None, None, ("-" ^ v) :: args
      | Some _ -> (* an error but this is reported during typed parsing *)
          None, value, args
      end
  | _ ->
      match value with
      | Some _ -> None, value, args
      | None -> (* Get it from the next argument. *)
          match args with
          | [] -> None, None, args
          | v :: rest when for_completion && has_complete_prefix v ->
              let v = get_token_to_complete v in
              if is_opt v then (* not an option value *) None, None, args else
              let comp =
                Cmdliner_def.Complete.make ~token:v (Opt_value arg_info)
              in
              Some comp, None, rest
          | v :: rest ->
              if is_opt v then None, None, args else None, Some v, rest

let try_complete_opt_value cline arg_info name value args =
  (* At that point we found a matching option name so this should be mostly
     about completing a glued option value, but there are twists. *)
  match Cmdliner_def.Arg_info.opt_kind arg_info with
  | Cmdliner_def.Arg_info.Flag ->
      begin match value with
      | Some v when is_short_opt name ->
          (* short flag dash sharing, push the completion *)
          let args = (complete_prefix ^ "-" ^ v) :: args in
          None, None, args
      | Some v ->
          (* This is actually a parse error, flags have no value.  We
             make it an option completion but the completions will
             eventually be empty (the prefix won't match) *)
          Some (Cmdliner_def.Complete.make ~token:(name ^ v) Opt_name),
          None, args
      | None ->
          (* We have in fact a fully completed flag turn it into an
             option completion. *)
          Some (Cmdliner_def.Complete.make ~token:name Opt_name), None, args
      end
  | _ ->
      begin match value with
      | Some token ->
          Some (Cmdliner_def.Complete.make ~token (Opt_value arg_info)), None,
          args
      | None ->
          (* We have a fully completed option name, we don't try to
             lookup what happens in the next argument which should
             hold the value if any, we just turn it into an option
             completion. *)
          Some (Cmdliner_def.Complete.make ~token:name Opt_name), None, args
      end

let parse_opt_args
    ~peek_opts ~legacy_prefixes ~for_completion optidx cline args
  =
  (* returns an updated [cline] cmdline according to the options found in [args]
     with the trie index [optidx]. Positional arguments are returned in order
     in a list. *)
  let rec loop errs k comp cline pargs = function
  | [] -> List.rev errs, comp, cline, false, List.rev pargs
  | "--" :: args ->
      List.rev errs, comp, cline, true, (List.rev_append pargs args)
  | s :: args ->
      let do_parse =
        is_opt s &&
        (if not for_completion then true else
         if not (has_complete_prefix s) then true else
         is_opt_to_complete s)
      in
      if not do_parse then loop errs (k + 1) comp cline (s :: pargs) args else
      let name, value, is_completion = parse_opt_arg s in
      match Cmdliner_trie.find ~legacy_prefixes optidx name with
      | Ok arg_info ->
          let acomp, value, args =
            if is_completion
            then try_complete_opt_value cline arg_info name value args
            else parse_opt_value ~for_completion cline arg_info name value args
          in
          let comp = match acomp with Some _ -> acomp | None -> comp in
          let arg : Cmdliner_def.Cline.arg =
            O ((k, name, value) ::
               Cmdliner_def.Cline.get_opt_arg cline arg_info)
          in
          let cline = Cmdliner_def.Cline.add arg_info arg cline in
          loop errs (k + 1) comp cline pargs args
      | Error (`Not_found | `Ambiguous) when for_completion ->
          if not is_completion then
            (* Drop the data, if the user thought this was an opt with
               an argument this may confuse positional args but there's
               not much we can do. *)
            loop errs (k + 1) comp cline pargs args
          else
          let token = name ^ Option.value ~default:"" value in
          let comp = Some (Cmdliner_def.Complete.make ~token Opt_name) in
          loop errs (k + 1) comp cline pargs args
      | Error `Not_found when peek_opts ->
          loop errs (k + 1) comp cline pargs args
      | Error `Not_found ->
          let hints = hint_matching_opt optidx s in
          let err = Cmdliner_base.err_unknown ~kind:"option" ~hints name in
          loop (err :: errs) (k + 1) comp cline pargs args
      | Error `Ambiguous (* Only on legacy prefixes *) ->
          let ambs = Cmdliner_trie.ambiguities optidx name in
          let ambs = List.sort compare ambs in
          let err = Cmdliner_base.err_ambiguous ~kind:"option" name ~ambs in
          loop (err :: errs) (k + 1) comp cline pargs args
  in
  let errs, comp, cline, has_dashdash, pargs = loop [] 0 None cline [] args in
  if errs = [] then Ok (comp, cline, has_dashdash, pargs) else
  match comp with
  | Some _ -> Ok (comp, cline, has_dashdash, pargs)
  | None ->
      let err = String.concat "\n" errs in
      Error (err, cline, has_dashdash, pargs)

(* Positional argument parsing *)

let take_range ~for_completion start stop l =
  let rec loop i comp acc = function
  | [] -> comp, (List.rev acc)
  | v :: vs ->
      if i < start then loop (i + 1) comp acc vs else
      if i <= stop then match maybe_token_to_complete ~for_completion v with
      | Some _ as comp -> loop (i + 1) comp (v :: acc) vs
      | None -> loop (i + 1) comp (v :: acc) vs
      else comp, List.rev acc
  in
  loop 0 None [] l

let parse_pos_args ~for_completion posidx comp cline ~has_dashdash pargs =
  (* returns an updated [cline] cmdline in which each positional arg mentioned
     in the list index [posidx], is given a value according the list
     of positional arguments values [pargs]. *)
  if pargs = [] then
    let misses = List.filter Cmdliner_def.Arg_info.is_req posidx in
    if misses = [] then Ok (comp, cline) else
    match comp with
    | Some _ -> Ok (comp, cline)
    | None -> Error (Cmdliner_msg.err_pos_misses misses, cline)
  else
  let last = List.length pargs - 1 in
  let pos rev k = if rev then last - k else k in
  let rec loop misses comp cline max_spec = function
  | [] -> misses, comp, cline, max_spec
  | arg_info :: al ->
      let apos = Cmdliner_def.Arg_info.pos_kind arg_info in
      let rev = Cmdliner_def.Arg_info.pos_rev apos in
      let start = pos rev (Cmdliner_def.Arg_info.pos_start apos) in
      let stop = match Cmdliner_def.Arg_info.pos_len apos with
      | None -> pos rev last
      | Some n -> pos rev (Cmdliner_def.Arg_info.pos_start apos + n - 1)
      in
      let start, stop = if rev then stop, start else start, stop in
      let comp, args = match take_range ~for_completion start stop pargs with
      | None, args -> comp, args
      | Some token, args ->
          let comp =
            Cmdliner_def.Complete.make ~after_dashdash:has_dashdash ~token
              (Opt_name_or_pos_value arg_info)
          in
          Some comp, args
      in
      let max_spec = max stop max_spec in
      let cline = Cmdliner_def.Cline.add arg_info (P args) cline in
      let misses = match Cmdliner_def.Arg_info.is_req arg_info && args = [] with
      | true -> arg_info :: misses
      | false -> misses
      in
      loop misses comp cline max_spec al
  in
  let misses, comp, cline, max_spec = loop [] comp cline (-1) posidx in
  if misses <> [] then begin
    if Option.is_some comp then Ok (comp, cline) else
    Error (Cmdliner_msg.err_pos_misses misses, cline)
  end else
  if last <= max_spec then Ok (comp, cline) else
  if Option.is_some comp then Ok (comp, cline) else
  let comp, excess = take_range ~for_completion (max_spec + 1) last pargs in
  match comp with
  | None -> Error (Cmdliner_msg.err_pos_excess excess, cline)
  | Some token ->
      let comp =
        Cmdliner_def.Complete.make ~after_dashdash:has_dashdash ~token Opt_name
      in
      Ok (Some comp, cline)

let create ?(peek_opts = false) ~legacy_prefixes ~for_completion al args =
  let optidx, posidx, cline = arg_info_indexes al in
  match
    parse_opt_args ~for_completion ~peek_opts ~legacy_prefixes optidx cline args
  with
  | Ok (comp, cline, _has_dashdash, _pargs) when peek_opts ->
      begin match comp with
      | None -> `Ok cline
      | Some comp -> `Complete (comp, cline)
      end
  | Ok (comp, cline, has_dashdash, pargs) ->
      begin match
        parse_pos_args ~for_completion posidx comp cline ~has_dashdash pargs
      with
      | Ok (None, _) | Error _ when for_completion ->
          (* Normally we should have found a completion token This
             may fail to happen if pos args are ill defined: we may miss the
             completion token. Just make sure we do a completion. *)
          begin match List.find_opt has_complete_prefix pargs with
          | None -> assert false
          | Some arg ->
              match maybe_token_to_complete ~for_completion:true arg with
              | None -> assert false
              | Some token ->
                  let comp =
                    Cmdliner_def.Complete.make
                      ~after_dashdash:has_dashdash ~token Opt_name
                  in
                  `Complete (comp, cline)
          end
      | Ok (None, cline) -> `Ok cline
      | Ok (Some comp, cline) -> `Complete (comp, cline)
      | Error v -> `Error v
      end
  | Error (errs, cline, has_dashdash, pargs) ->
      match
        parse_pos_args ~for_completion posidx None cline ~has_dashdash pargs
      with
      | Ok (Some comp, cline) -> `Complete (comp, cline)
      | _ -> `Error (errs, cline)
