open Import
open Memo.O
open Dune_pkg

type t =
  { os : string option Memo.Lazy.t
  ; os_version : string option Memo.Lazy.t
  ; os_distribution : string option Memo.Lazy.t
  ; os_family : string option Memo.Lazy.t
  ; arch : string option Memo.Lazy.t
  ; sys_ocaml_version : string option Memo.Lazy.t
  }

let os t (v : Dune_lang.Pform.Var.Os.t) =
  Memo.Lazy.force
    (match v with
     | Os -> t.os
     | Os_version -> t.os_version
     | Os_distribution -> t.os_distribution
     | Os_family -> t.os_family)
;;

let poll =
  let vars =
    lazy
      (let path = Env_path.path (Global.env ()) in
       Sys_poll.make ~path)
  in
  let sys_poll_memo key =
    Memo.lazy_ ~name:"sys-poll-vars" ~cutoff:(Option.equal String.equal) (fun () ->
      let vars = Lazy.force vars in
      Memo.of_reproducible_fiber @@ key vars)
  in
  { os = sys_poll_memo Sys_poll.os
  ; os_version = sys_poll_memo Sys_poll.os_version
  ; os_distribution = sys_poll_memo Sys_poll.os_distribution
  ; os_family = sys_poll_memo Sys_poll.os_family
  ; arch = sys_poll_memo Sys_poll.arch
  ; sys_ocaml_version = sys_poll_memo Sys_poll.sys_ocaml_version
  }
;;

let os_values t v =
  let+ v = os t v in
  [ Value.String (Option.value v ~default:"") ]
;;

(* These values are needed before the selected compiler is available. *)
let expand t ~(source : Dune_sexp.Template.Pform.t) (pform : Pform.t) =
  match pform with
  | Macro _ ->
    User_error.raise
      ~loc:source.loc
      [ Pp.text "Macros are not allowed in this position." ]
  | Var var ->
    (match var with
     | Architecture ->
       let+ arch = Memo.Lazy.force t.arch in
       (match arch with
        | Some "x86_64" -> Some "amd64"
        | other -> other)
     | System ->
       let+ os = Memo.Lazy.force t.os in
       (match os with
        | Some "macos" -> Some "macosx"
        | other -> other)
     | _ ->
       User_error.raise
         ~loc:source.loc
         [ Pp.textf
             "%s isn't allowed in this position."
             (Dune_sexp.Template.Pform.describe source)
         ])
;;

let solver_env =
  Memo.Lazy.create ~name:"solver-env" (fun () ->
    List.combine
      Package_variable_name.
        [ os; os_version; os_distribution; os_family; arch; sys_ocaml_version ]
      (let { os; os_version; os_distribution; os_family; arch; sys_ocaml_version } =
         poll
       in
       [ os; os_version; os_distribution; os_family; arch; sys_ocaml_version ])
    |> Memo.List.filter_map ~f:(fun (var, value) ->
      Memo.Lazy.force value
      >>| Option.map ~f:(fun value -> var, Variable_value.string value))
    >>| List.fold_left ~init:Solver_env.empty ~f:(fun acc (var, value) ->
      Solver_env.set acc var value))
  |> Memo.Lazy.force
;;
