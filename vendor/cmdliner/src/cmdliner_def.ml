(*---------------------------------------------------------------------------
   Copyright (c) 2011 The cmdliner programmers. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

let strf = Printf.sprintf

(* Exit codes *)

module Exit = struct
  type code = int

  let ok = 0
  let some_error = 123
  let cli_error = 124
  let internal_error = 125

  type info =
    { codes : code * code; (* min, max *)
      doc : string; (* help. *)
      docs : string; } (* title of help section where listed. *)

  let info
      ?(docs = Cmdliner_manpage.s_exit_status) ?(doc = "undocumented") ?max min
    =
    let max = match max with None -> min | Some max -> max in
    { codes = (min, max); doc; docs }

  let info_codes i = i.codes
  let info_code i = fst i.codes
  let info_doc i = i.doc
  let info_docs i = i.docs
  let info_order i0 i1 = compare i0.codes i1.codes
  let defaults =
    [ info ok ~doc:"on success.";
      info some_error
        ~doc:"on indiscriminate errors reported on standard error.";
      info cli_error ~doc:"on command line parsing errors.";
      info internal_error ~doc:"on unexpected internal errors (bugs)."; ]

  let doclang_subst ~subst i = function
  | "status" -> Some (string_of_int (info_code i))
  | "status_max" -> Some (string_of_int (snd i.codes))
  | id -> subst id
end

(* Environment variables *)

module Env = struct
  type var = string
  type info = (* information about an environment variable. *)
    { id : int; (* unique id for the env var. *)
      deprecated : string option;
      var : string; (* the variable. *)
      doc : string; (* help. *)
      docs : string; } (* title of help section where listed. *)

  let info
      ?deprecated
      ?(docs = Cmdliner_manpage.s_environment) ?(doc = "See option $(opt).") var
    =
    { id = Cmdliner_base.uid (); deprecated; var; doc; docs }

  let info_deprecated i = i.deprecated
  let info_var i = i.var
  let info_doc i = i.doc
  let info_docs i = i.docs
  let info_compare i0 i1 = Int.compare i0.id i1.id

  let doclang_subst ~subst i = function
  | "env" -> Some (strf "$(b,%s)" (Cmdliner_manpage.escape i.var))
  | id -> subst id

  let styled_deprecated ~errs ~subst i = match i.deprecated with
  | None -> "" | Some msg -> Cmdliner_manpage.doc_to_styled ~errs ~subst msg

  let styled_doc ~errs ~subst i =
    Cmdliner_manpage.doc_to_styled ~errs ~subst i.doc

  module Set = Set.Make (struct type t = info let compare = info_compare end)
end

(* Argument information *)

module Arg_info = struct
  type absence = Err | Val of string Lazy.t | Doc of string
  type opt_kind = Flag | Opt | Opt_vopt of string
  type pos_kind = (* information about a positional argument. *)
    { pos_rev : bool; (* if [true] positions are counted from the end. *)
      pos_start : int; (* start positional argument. *)
      pos_len : int option } (* number of arguments or [None] if unbounded. *)

  let pos ~rev:pos_rev ~start:pos_start ~len:pos_len =
    { pos_rev; pos_start; pos_len}

  let pos_rev p = p.pos_rev
  let pos_start p = p.pos_start
  let pos_len p = p.pos_len
  let dumb_pos = pos ~rev:false ~start:(-1) ~len:None

  type t = (* information about a command line argument. *)
    { id : int; (* unique id for the argument. *)
      deprecated : string option; (* deprecation message *)
      absent : absence; (* behaviour if absent. *)
      env : Env.info option; (* environment variable for default value. *)
      doc : string; (* help. *)
      docv : string; (* variable name for the argument in help. *)
      doc_envs : Env.info list; (* environment that needs to be added to docs *)
      docs : string; (* title of help section where listed. *)
      pos : pos_kind; (* positional arg kind. *)
      opt_kind : opt_kind; (* optional arg kind. *)
      opt_names : string list; (* names (for opt args). *)
      opt_all : bool; } (* repeatable (for opt args). *)

  let make
      ?deprecated ?(absent = "") ?docs ?(doc_envs = []) ?(docv = "")
      ?(doc = "") ?env names
    =
    let dash n = if String.length n = 1 then "-" ^ n else "--" ^ n in
    let opt_names = List.map dash names in
    let docs = match docs with
    | Some s -> s
    | None ->
        match names with
        | [] -> Cmdliner_manpage.s_arguments
        | _ -> Cmdliner_manpage.s_options
    in
    { id = Cmdliner_base.uid (); deprecated; absent = Doc absent;
      env; doc; docv; doc_envs; docs; pos = dumb_pos;
      opt_kind = Flag; opt_names; opt_all = false; }

  let id i = i.id
  let deprecated i = i.deprecated
  let absent i = i.absent
  let env i = i.env
  let doc i = i.doc
  let docv i = i.docv
  let doc_envs i = i.doc_envs
  let docs i = i.docs
  let pos_kind i = i.pos
  let opt_kind i = i.opt_kind
  let opt_names i = i.opt_names
  let opt_all i = i.opt_all
  let opt_name_sample i =
    (* First long or short name (in that order) in the list; this
       allows the client to control which name is shown *)
    let rec find = function
    | [] -> List.hd i.opt_names
    | n :: ns -> if (String.length n) > 2 then n else find ns
    in
    find i.opt_names

  let make_req i = { i with absent = Err }
  let make_all_opts i = { i with opt_all = true }
  let make_opt ~docv ~absent ~kind:opt_kind i =
    { i with absent; opt_kind; docv }

  let make_opt_all ~docv ~absent ~kind:opt_kind i =
    { i with absent; opt_kind; opt_all = true; docv  }

  let make_pos ~docv ~pos i = { i with pos; docv }
  let make_pos_abs ~docv ~absent ~pos i = { i with absent; pos; docv }

  let is_opt i = i.opt_names <> []
  let is_pos i = i.opt_names = []
  let is_req i = i.absent = Err

  let pos_cli_order (a0 : t) (a1 : t) = (* best-effort order on the cli. *)
    let c = Bool.compare (a0.pos.pos_rev) (a1.pos.pos_rev) in
    if c <> 0 then c else
    if a0.pos.pos_rev
    then Int.compare a1.pos.pos_start a0.pos.pos_start
    else Int.compare a0.pos.pos_start a1.pos.pos_start

  let rev_pos_cli_order a0 a1 = pos_cli_order a1 a0

  let doclang_subst ~subst (i : t) = function
  | "docv" ->
      let docv = if i.docv = "" then "VAL" else i.docv in
      Some (strf "$(i,%s)" (Cmdliner_manpage.escape docv))
  | "opt" when is_opt i ->
      Some (strf "$(b,%s)" (Cmdliner_manpage.escape (opt_name_sample i)))
  | id ->
      match env i with
      | Some e -> Env.doclang_subst ~subst e id
      | None -> subst id

  let styled_deprecated ~errs ~subst (i : t) = match i.deprecated with
  | None -> "" | Some msg -> Cmdliner_manpage.doc_to_styled ~errs ~subst msg

  let styled_doc ~errs ~subst (i : t) =
    Cmdliner_manpage.doc_to_styled ~errs ~subst i.doc

  let compare (a0 : t) (a1 : t) = Int.compare a0.id a1.id
  module Map = Map.Make (struct type nonrec t = t let compare = compare end)

  (* Due to terms appearing in the completion API, we have an annoying
     recursive type definition which we resolve here. Most of these
     types do not belong this module. *)

  type term_escape =
  [ `Error of bool * string
  | `Help of Cmdliner_manpage.format * string option ]

  type 'a completion_directive =
  | Message of string | String of string * string | Value of 'a * string
  | Files | Dirs | Restart | Raw of string

  type ('ctx, 'a) completion_func =
    'ctx option -> token:string -> ('a completion_directive list, string) result

  type 'a parser = string -> ('a, string) result
  type 'a complete =
  | Complete : 'ctx term option * ('ctx, 'a) completion_func -> 'a complete

  and 'a completion = { complete : 'a complete }

  and 'a conv =
    { docv : string;
      parser : 'a parser;
      pp : 'a Cmdliner_base.Fmt.t;
      completion : 'a completion; }

  and e_conv = Conv : 'a conv -> e_conv
  and arg_set = e_conv Map.t
  and cmd =
    { name : string; (* name of the cmd. *)
      version : string option; (* version (for --version). *)
      deprecated : string option; (* deprecation message *)
      doc : string; (* one line description of cmd. *)
      docs : string; (* title of man section where listed (commands). *)
      sdocs : string; (* standard options, title of section where listed. *)
      exits : Exit.info list; (* exit codes for the cmd. *)
      envs : Env.info list; (* env vars that influence the cmd. *)
      man : Cmdliner_manpage.block list; (* man page text. *)
      man_xrefs : Cmdliner_manpage.xref list; (* man cross-refs. *)
      args : arg_set; (* Command arguments. *)
      has_args : bool; (* [true] if has own parsing term. *)
      children : cmd list; } (* Children, if any. *)

  and eval = (* information about the evaluation context. *)
    { cmd : cmd; (* cmd being evaluated. *)
      ancestors : cmd list; (* ancestors of cmd, root is last. *)
      subcmds : cmd list; (* subcommands (if any) *)
      env : string -> string option; (* environment variable lookup. *)
      err_ppf : Format.formatter (* error formatter *) }

  and cline = cline_arg Map.t
  and cline_arg = (* unconverted argument data as found on the command line. *)
  | O of (int * string * (string option)) list (* (pos, name, value) of opt. *)
  | P of string list

  and 'a term_parser =
    eval -> cline -> ('a, [ `Parse of string | term_escape ]) result

  and 'a term = arg_set * 'a term_parser

  (* Sets of arguments stored as maps to their completion *)

  module Set = struct
    include Map
    type t = e_conv Map.t
    let find_opt k m = try Some (Map.find k m) with Not_found -> None
    let elements m = List.map fst (bindings m)
    let union a b =
      Map.merge (fun k v v' ->
        match v, v' with
        | Some v, _ | _, Some v -> Some v
        | None, None -> assert false) a b
  end
end

(* Commands *)

module Cmd_info = struct
  type t = Arg_info.cmd
  let make
      ?deprecated ?(man_xrefs = [`Main]) ?(man = []) ?(envs = [])
      ?(exits = Exit.defaults) ?(sdocs = Cmdliner_manpage.s_common_options)
      ?(docs = Cmdliner_manpage.s_commands) ?(doc = "") ?version name : t
    =
    { name; version; deprecated; doc; docs; sdocs; exits;
      envs; man; man_xrefs; args = Arg_info.Set.empty;
      has_args = true; children = [] }

  let name (i : t) = i.name
  let version (i : t) = i.version
  let deprecated (i : t) = i.deprecated
  let doc (i : t) = i.doc
  let docs (i : t) = i.docs
  let stdopts_docs (i : t) = i.sdocs
  let exits (i : t) = i.exits
  let envs (i : t) = i.envs
  let man (i : t) = i.man
  let man_xrefs (i : t) = i.man_xrefs
  let args (i : t) = i.args
  let has_args (i : t) = i.has_args
  let children (i : t) = i.children
  let add_args (i : t) args = { i with args = Arg_info.Set.union args i.args }
  let with_children (i : t) ~args ~children =
    let has_args, args = match args with
    | None -> false, i.args
    | Some args -> true, Arg_info.Set.union args i.args
    in
    { i with has_args; args; children }

  let styled_deprecated ~errs ~subst (i : t) = match i.deprecated with
  | None -> "" | Some msg -> Cmdliner_manpage.doc_to_styled ~errs ~subst msg

  let styled_doc ~errs ~subst (i : t) =
    Cmdliner_manpage.doc_to_styled ~errs ~subst i.doc

  let escaped_name (i : t) = Cmdliner_manpage.escape i.name
end

(* Command lines *)

module Cline = struct
  type arg = Arg_info.cline_arg =
  | O of (int * string * (string option)) list
  | P of string list

  type t = Arg_info.cline

  let empty = Arg_info.Map.empty
  let add = Arg_info.Map.add
  let fold = Arg_info.Map.fold
  let get_arg cline a : arg =
    try Arg_info.Map.find a cline with Not_found -> assert false

  let get_opt_arg cline a =
    match get_arg cline a with O l -> l | _ -> assert false

  let get_pos_arg cline a =
    match get_arg cline a with P l -> l | _ -> assert false

  let actual_args cline a = match get_arg cline a with
  | P args -> args
  | O l ->
      let extract_args (_pos, name, value) =
        name :: (match value with None -> [] | Some v -> [v])
      in
      List.concat (List.map extract_args l)

  (* Deprecations *)

  type deprecated = Arg_info.t * arg

  let deprecated ~env cline =
    let add ~env info arg acc =
      let deprecation_invoked = match (arg : arg) with
      | O [] | P [] -> (* nothing on the cli for the argument *)
          begin match Arg_info.env info with
          | None -> false
          | Some ienv ->
              (* the parse uses the env var if defined which may be
                 deprecated  *)
              Option.is_some (Env.info_deprecated ienv) &&
              Option.is_some (env (Env.info_var ienv))
          end
      | _ -> Option.is_some (Arg_info.deprecated info)
      in
      if deprecation_invoked then (info, arg) :: acc else acc
    in
    List.rev (fold (add ~env) cline [])

  let pp_deprecated ~subst ppf (info, arg) =
    let open Cmdliner_base in
  let plural l = if List.length l > 1 then "s" else "" in
    let subst = Arg_info.doclang_subst ~subst info in
    match (arg : arg) with
    | O [] | P [] ->
        let env = Option.get (Arg_info.env info) in
        let msg = Env.styled_deprecated ~errs:ppf ~subst env in
        Fmt.pf ppf "@[%a @[environment variable %a: %a@]@]"
          Fmt.deprecated () Fmt.code (Env.info_var env)
          Fmt.styled_text msg
    | O os ->
        let plural = plural os in
        let names = List.map (fun (_, n, _) -> n) os in
        let msg = Arg_info.styled_deprecated ~errs:ppf ~subst info in
        Fmt.pf ppf "@[%a @[option%s %a: %a@]@]"
          Fmt.deprecated () plural Fmt.(list ~sep:sp code_or_quote) names
          Fmt.styled_text msg
    | P args ->
        let plural = plural args in
        let msg =
          Arg_info.styled_deprecated ~errs:ppf ~subst info
        in
        Fmt.pf ppf "@[%a @[argument%s %a: %a@]@]"
          Fmt.deprecated () plural Fmt.(list ~sep:sp code_or_quote) args
          Fmt.styled_text msg
end

(* Evaluation *)

module Eval = struct
  type t = Arg_info.eval

  let make ~ancestors ~cmd ~subcmds ~env ~err_ppf : t =
    { ancestors; cmd; subcmds; env; err_ppf }

  let cmd (i : t) = i.cmd
  let ancestors (i : t) = i.ancestors
  let subcmds (i : t) = i.subcmds
  let env_var (i : t) v = i.env v
  let err_ppf (i : t) = i.err_ppf
  let main (i : t) = match List.rev i.ancestors with [] -> i.cmd | m :: _ -> m
  let with_cmd (i : t) cmd = { i with cmd }

  let doclang_name n = strf "$(b,%s)" (Cmd_info.escaped_name n)
  let doclang_names names =
    strf "$(b,%s)" (Cmdliner_manpage.escape (String.concat " " names))

  let doclang_subst (i : t) = function
  | "tname" | "cmd.name" -> Some (doclang_name i.cmd)
  | "mname" | "tool" -> Some (doclang_name (main i))
  | "cmd.parent" ->
      let ancestors = ancestors i in
      if ancestors = [] then Some (doclang_name (main i)) else
      Some (doclang_names (List.rev_map Cmd_info.name ancestors))
  | "iname" | "cmd" ->
      Some (doclang_names (List.rev_map Cmd_info.name (cmd i :: ancestors i)))
  | _ -> None
end

(* Terms *)

module Term = struct
  type escape = Arg_info.term_escape
  type 'a parser = 'a Arg_info.term_parser
  type 'a t = 'a Arg_info.term
  let some (aset, parser) =
    aset, (fun eval cline -> Result.map Option.some (parser eval cline))
end

module Arg_completion = struct
  type 'a directive = 'a Arg_info.completion_directive =
  | Message of string | String of string * string | Value of 'a * string
  | Files | Dirs | Restart | Raw of string

  let value ?(doc = "") v = Value (v, doc)
  let string ?(doc = "") s = String (s, doc)
  let files = Files
  let dirs = Dirs
  let restart = Restart
  let message msg = Message msg
  let raw s = Raw s

  type ('ctx, 'a) func =
    'ctx option -> token:string -> ('a directive list, string) result

  type 'a complete = 'a Arg_info.complete =
  | Complete : 'ctx Term.t option * ('ctx, 'a) func -> 'a complete

  type 'a t = 'a Arg_info.completion

  let make ?context func : 'a t = { complete = Complete (context, func) }
  let complete (c : 'a t) = c.complete

  let complete_files : 'a t =
    { complete = Complete (None, fun _ ~token:_ -> Ok [Files]) }

  let complete_dirs : 'a t =
    { complete = Complete (None, fun _ ~token:_ -> Ok [Dirs]) }

  let complete_paths : 'a t =
    { complete = Complete (None, fun _ ~token:_ -> Ok [Files; Dirs]) }

  let complete_restart : 'a t =
    { complete = Complete (None, fun _ ~token:_ -> Ok [Restart]) }

  let complete_none : 'a t =
    { complete = Complete (None, fun _ ~token:_ -> Ok []) }

  let directive_some : 'a directive -> 'a option directive = function
  | Value (v, doc) -> Value (Some v, doc)
  | (Message _ | String _ | Files | Dirs | Restart | Raw _ as v) -> v

  let complete_some (c : 'a t) : 'a option t = match c.complete with
  | Complete (ctx, func) ->
      let func ctx ~token =
        let some_result directives = List.map directive_some directives in
        Result.map some_result (func ctx ~token)
      in
      { complete = Complete (ctx, func) }
end

(* Converters *)

module Arg_conv = struct
  type 'a parser = 'a Arg_info.parser
  type 'a fmt = 'a Cmdliner_base.Fmt.t
  type 'a t = 'a Arg_info.conv

  let make
      ?(completion = Arg_completion.complete_none) ~docv ~parser ~pp () : 'a t =
    { docv; parser; pp; completion }

  let of_conv ?completion ?docv ?parser ?pp (conv : 'a t) : 'a t
    =
    let completion = Option.value ~default:conv.completion completion in
    let docv = Option.value ~default:conv.docv docv in
    let parser = Option.value ~default:conv.parser parser in
    let pp = Option.value ~default:conv.pp pp in
    { docv; parser; pp; completion }

  let docv (c : 'a t) = c.docv
  let parser (c : 'a t) = c.parser
  let pp (c : 'a t) = c.pp
  let completion (c : 'a t) = c.completion

  let none : 'a t =
    { docv = "";
      parser = (fun _ -> assert false);
      pp = (fun _ _ -> assert false);
      completion = Arg_completion.complete_none }

  let some ?(none = "") conv =
    let parser s = Result.map Option.some (parser conv s) in
    let pp ppf v = match v with
    | None -> Format.pp_print_string ppf none
    | Some v -> pp conv ppf v
    in
    let completion = Arg_completion.complete_some (completion conv) in
    { conv with parser; pp; completion }

  let some' ?none conv =
    let parser s = Result.map Option.some (parser conv s) in
    let pp ppf = function
    | None -> (match none with None -> () | Some v -> (pp conv) ppf v)
    | Some v -> pp conv ppf v
    in
    let completion = Arg_completion.complete_some conv.completion in
    { conv with parser; pp; completion }
end

(* Completion *)

module Complete = struct
  type kind =
  | Opt_value of Arg_info.t
  | Opt_name_or_pos_value of Arg_info.t
  | Opt_name

  type t =
    { token : string;
      after_dashdash : bool;
      subcmds : bool; (* Note this is adjusted in Cmdliner_eval *)
      kind : kind }

  let make ?(after_dashdash = false) ?(subcmds = false) ~token kind =
    { token; after_dashdash; subcmds; kind; }

  let token c = c.token
  let after_dashdash c = c.after_dashdash
  let subcmds c = c.subcmds
  let kind c = c.kind
  let add_subcmds c = { c with subcmds = true }
end
