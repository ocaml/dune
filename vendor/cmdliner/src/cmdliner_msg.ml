(*---------------------------------------------------------------------------
   Copyright (c) 2011 The cmdliner programmers. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

module Fmt = Cmdliner_base.Fmt

(* Environment variable errors *)

let err_env_parse env ~err =
  let var = Cmdliner_def.Env.info_var env in
  Fmt.str "@[environment variable %a: %s@]" Fmt.code_or_quote var err

(* Positional argument errors *)

let err_pos_excess excess =
  Fmt.str "@[%a, don't know what to do with %a@]"
    Fmt.ereason "too many arguments"
    Fmt.(list ~sep:comma code_or_quote) excess

let err_pos_miss a = match Cmdliner_def.Arg_info.docv a with
| "" -> Fmt.str "@[a required argument is %a@]" Fmt.missing ()
| v -> Fmt.str "@[required argument %a is %a@]" Fmt.code_var v Fmt.missing ()

let err_pos_misses = function
| [] -> assert false
| [a] -> err_pos_miss a
| args ->
    let add_arg acc a = match Cmdliner_def.Arg_info.docv a with
    | "" -> "ARG" :: acc
    | argv -> argv :: acc
    in
    let rev_args = List.sort Cmdliner_def.Arg_info.rev_pos_cli_order args in
    let args = List.fold_left add_arg [] rev_args in
    Fmt.str "@[required arguments %a@ are@ %a@]"
      Fmt.(list ~sep:comma code_var) args Fmt.missing ()

let err_pos_parse a ~err = match Cmdliner_def.Arg_info.docv a with
| "" -> err
| argv ->
    match Cmdliner_def.Arg_info.(pos_len @@ pos_kind a) with
    | Some 1 -> Fmt.str "@[%a argument: %s@]" Fmt.code_var argv err
    | None | Some _ -> Fmt.str "@[%a… arguments: %s@]" Fmt.code_var argv err

(* Optional argument errors *)

let err_flag_value flag v =
  Fmt.str "@[option %a is a flag, it@ %a@ %a@]"
    Fmt.code_or_quote flag Fmt.ereason "cannot take the argument"
    Fmt.code_or_quote v

let err_opt_value_missing f =
  Fmt.str "@[option %a %a@]" Fmt.code_or_quote f Fmt.ereason "needs an argument"

let err_opt_parse f ~err =
  Fmt.str "@[option %a: %a@]" Fmt.code_or_quote f Fmt.styled_text err

let err_opt_repeated f f' =
  if f = f' then
    Fmt.str "@[option %a %a@]"
      Fmt.code_or_quote f Fmt.ereason "cannot be repeated"
  else
  Fmt.str "@[options %a and %a@ %a@]"
    Fmt.code_or_quote f Fmt.code_or_quote f'
    Fmt.ereason "cannot be present at the same time"

(* Argument errors *)

let err_arg_missing a =
  if Cmdliner_def.Arg_info.is_pos a then err_pos_miss a else
  Fmt.str "@[required option %a is %a@]"
    Fmt.code (Cmdliner_def.Arg_info.opt_name_sample a) Fmt.missing ()

let err_cmd_missing ~dom =
  Fmt.str "@[required %a name is %a,@ must@ be@ %a@]"
    Fmt.code_var "COMMAND" Fmt.missing () Cmdliner_base.pp_alts dom

(* Other messages *)

let pp_version ppf ei =
  match Cmdliner_def.Cmd_info.version (Cmdliner_def.Eval.main ei) with
  | None -> assert false
  | Some v -> Fmt.pf ppf "@[%s@]@." v

let exec_name ei = Cmdliner_def.Cmd_info.name (Cmdliner_def.Eval.main ei)

let pp_exec_msg ppf ei = Fmt.pf ppf "%s:" (exec_name ei)

let pp_err ppf ei ~err =
  Fmt.pf ppf "@[%a @[%a@]@]@." pp_exec_msg ei Fmt.styled_text err

let pp_usage_and_err ppf ei ~err =
  Fmt.pf ppf "@[Usage: @[%a@]@]@."
    Fmt.styled_text (Cmdliner_docgen.styled_usage_synopsis ~errs:ppf ei);
  pp_err ppf ei ~err

let pp_backtrace ppf ei e bt =
  let bt = Printexc.raw_backtrace_to_string bt in
  let bt =
    let len = String.length bt in
    if len > 0 then String.sub bt 0 (len - 1) (* remove final '\n' *) else bt
  in
  Fmt.pf ppf "@[%a @[internal error, %a:@\n%a@]@]@."
    pp_exec_msg ei
    Fmt.ereason "uncaught exception"
    Fmt.lines (String.concat "\n" [Printexc.to_string e; bt])
