open Import
open Dune_lang.Decoder

module Mode = struct
  include Rule.Mode
  include Rule_mode_decoder
end

type t =
  { targets : String_with_vars.t Targets_spec.t
  ; deps : Dep_conf.t Bindings.t
  ; action : Loc.t * Dune_lang.Action.t
  ; mode : Rule_mode.t
  ; locks : Locks.t
  ; loc : Loc.t
  ; enabled_if : Blang.t
  ; aliases : Alias.Name.t list
  ; package : Package.t option
  }

include Stanza.Make (struct
    type nonrec t = t

    include Poly
  end)

type action_or_field =
  | Action
  | Field
  | Since of Syntax.Version.t * action_or_field

let atom_table =
  String.Map.of_list_exn
    [ "run", Action
    ; "chdir", Action
    ; "setenv", Action
    ; "with-stdout-to", Action
    ; "with-stderr-to", Action
    ; "with-outputs-to", Action
    ; "with-stdin-from", Action
    ; "ignore-stdout", Action
    ; "ignore-stderr", Action
    ; "ignore-outputs", Action
    ; "progn", Action
    ; "echo", Action
    ; "cat", Action
    ; "copy", Action
    ; "copy#", Action
    ; "copy-and-add-line-directive", Action
    ; "system", Action
    ; "bash", Action
    ; "write-file", Action
    ; "diff", Action
    ; "diff?", Action
    ; "targets", Field
    ; "target", Field
    ; "deps", Field
    ; "action", Field
    ; "locks", Field
    ; "fallback", Field
    ; "mode", Field
    ; "aliases", Field
    ; "alias", Field
    ; "enabled_if", Field
    ; "format-dune-file", Since ((3, 18), Action)
    ; "package", Since ((3, 8), Field)
    ]
;;

let short_form ~loc =
  let+ action = located Dune_lang.Action.decode_dune_file in
  { targets = Infer
  ; deps = Bindings.empty
  ; action
  ; mode = Standard
  ; locks = []
  ; loc
  ; enabled_if = Blang.true_
  ; aliases = []
  ; package = None
  }
;;

let directory_targets_extension =
  let syntax =
    Dune_lang.Syntax.create
      ~name:"directory-targets"
      ~desc:"experimental support for directory targets"
      ~experimental:true
      [ (0, 1), `Since (3, 0) ]
  in
  Dune_project.Extension.register syntax (Dune_lang.Decoder.return ((), [])) Dyn.unit
;;

let long_form ~loc =
  fields
  @@
  let* deps = field "deps" (Bindings.decode Dep_conf.decode) ~default:Bindings.empty in
  let* project = Dune_project.get_exn () in
  let allow_directory_targets =
    Dune_project.is_extension_set project directory_targets_extension
  in
  String_with_vars.add_user_vars_to_decoding_env
    (Bindings.var_names deps)
    (let+ action_o = field_o "action" (located Dune_lang.Action.decode_dune_file)
     and+ targets = Targets_spec.field ~allow_directory_targets
     and+ locks = Locks.field ()
     and+ () =
       let+ fallback =
         field_b
           ~check:
             (Dune_lang.Syntax.renamed_in Stanza.syntax (1, 0) ~to_:"(mode fallback)")
           "fallback"
       in
       (* The "fallback" field was only allowed in jbuild file, which we don't
          support anymore. So this cannot be [true]. We just keep the parser
          to provide a nice error message for people switching from jbuilder
          to dune. *)
       assert (not fallback)
     and+ mode = Mode.field
     and+ enabled_if = Enabled_if.decode ~allowed_vars:Any ~since:(Some (1, 4)) ()
     and+ package =
       Stanza_pkg.field_opt ~check:(Dune_lang.Syntax.since Stanza.syntax (2, 0)) ()
       >>| Option.map ~f:snd
     and+ aliases =
       let open Dune_sexp.Decoder in
       fields_mutually_exclusive
         ~default:[]
         [ ( "alias"
           , Dune_lang.Syntax.since Stanza.syntax (2, 0)
             >>> Dune_lang.Alias.decode
             >>| List.singleton )
         ; ( "aliases"
           , Dune_lang.Syntax.since Stanza.syntax (3, 5) >>> repeat Dune_lang.Alias.decode
           )
         ]
     in
     let action =
       match action_o with
       | Some action -> action
       | None ->
         let hints =
           if List.is_empty aliases
           then []
           else
             [ Pp.text "You can use the (alias) stanza to add dependencies to an alias." ]
         in
         field_missing ~hints loc "action"
     in
     { targets; deps; action; mode; locks; loc; enabled_if; aliases; package })
;;

let decode =
  let rec interpret ~loc atom = function
    | Field -> long_form ~loc
    | Action -> short_form ~loc
    | Since (version, inner) ->
      let what = Printf.sprintf "'%s' in short-form 'rule'" atom in
      Dune_lang.Syntax.since ~what Stanza.syntax version >>> interpret ~loc atom inner
  in
  let* stanza_loc = loc in
  peek_exn
  >>= function
  | List (_, Atom (loc, A s) :: _) ->
    (match String.Map.find atom_table s with
     | None ->
       User_error.raise
         ~loc
         [ Pp.text "Unknown action or rule field." ]
         ~hints:(User_message.did_you_mean s ~candidates:(String.Map.keys atom_table))
     | Some w -> interpret ~loc:stanza_loc s w)
  | sexp ->
    User_error.raise
      ~loc:(Dune_lang.Ast.loc sexp)
      [ Pp.textf "S-expression of the form (<atom> ...) expected" ]
;;
