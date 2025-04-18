open Import
open Dune_lang.Decoder
module Base_targets_spec = Targets_spec


module Mode = struct
  include Rule.Mode
  include Rule_mode_decoder
end

type t =
  { targets : String_with_vars.t Dune_lang.Targets_spec.t
  ; deps : Dep_conf.t Bindings.t
  ; action : Loc.t * Dune_lang.Action.t
  ; mode : Rule.Mode.t
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

let short_form =
  let+ loc, action = located Dune_lang.Action.decode_dune_file in
  { targets = Infer
  ; deps = Bindings.empty
  ; action = loc, action
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

let long_form =
  let* deps =
    let decode_deps =
      let open Dune_sexp.Decoder in
      peek_exn >>= function
      | List (_, (_ :: _ :: _)) -> (* Named bindings case *)
        Bindings.decode String_with_vars.decode >>| fun bindings ->
        (* Convert bindings to a single String_with_vars representing all dependencies *)
        let string_with_vars_to_string sw =
          match String_with_vars.text_only sw with
          | Some text -> text
          | None -> 
              let loc = String_with_vars.loc sw in
              User_error.raise ~loc
                [ Pp.text "Cannot use variables in dependency specifications" ]
        in
        let strings = List.concat_map bindings ~f:(function
          | Bindings.Unnamed sw -> 
              [string_with_vars_to_string sw]
          | Bindings.Named (name, sw_list) ->
              List.map sw_list ~f:(fun sw ->
                sprintf "%s:%s" name (string_with_vars_to_string sw)))
        in
        String_with_vars.make_text Loc.none (String.concat ~sep:" " strings)
      | _ -> (* Simple string case *)
        String_with_vars.decode
    in
    Base_targets_spec.field
      ~allow_directory_targets:true
      "deps"
      decode_deps
  in
  let* project = Dune_project.get_exn () in
  let allow_directory_targets =
    Dune_project.is_extension_set project directory_targets_extension
  in
  String_with_vars.add_user_vars_to_decoding_env
    (match deps with
     | Base_targets_spec.Infer -> []
     | Base_targets_spec.Static { targets; _ } ->
       targets 
       |> List.map ~f:fst 
       |> List.concat_map ~f:(fun sw ->
            match String_with_vars.text_only sw with
            | Some text -> [text]
            | None -> [])
    )
    (let+ loc = loc
     and+ action_o = field_o "action" (located Dune_lang.Action.decode_dune_file)
     and+ targets =
      Base_targets_spec.field ~allow_directory_targets "targets" String_with_vars.decode
     and+ locks = Locks.field ()
     and+ () =
       let+ fallback =
         field_b
           ~check:
             (Dune_lang.Syntax.renamed_in Stanza.syntax (1, 0) ~to_:"(mode fallback)")
           "fallback"
       in
       assert (not fallback)
     and+ mode = Mode.field
     and+ enabled_if = Enabled_if.decode ~allowed_vars:Any ~since:(Some (1, 4)) ()
     and+ package =
       field_o
         "package"
         (Dune_lang.Syntax.since Stanza.syntax (2, 0) >>> Stanza_common.Pkg.decode)
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
     let targets =
       match targets with
        | Base_targets_spec.Infer -> Base_targets_spec.Infer
        | Base_targets_spec.Static { targets; multiplicity; named_targets = _ } ->
          Base_targets_spec.Static { targets; multiplicity; named_targets = [] }
     in
     let deps : Dep_conf.t Bindings.t =
      []
     in
     { targets; deps; action; mode; locks; loc; enabled_if; aliases; package })

[@@@warning "-32"]

let targets =
  let* project = Dune_project.get_exn () in
  let allow_directory_targets =
    Dune_project.is_extension_set project directory_targets_extension
  in
  let+ targets = 
    Base_targets_spec.field 
      ~allow_directory_targets 
      "targets"  (* Field name *)
      String_with_vars.decode  (* Value decoder *)
  in
  match targets with
  | Base_targets_spec.Infer -> Base_targets_spec.Infer
  | Base_targets_spec.Static { targets; multiplicity; named_targets = _ } ->
    Base_targets_spec.Static { targets; multiplicity; named_targets = [] }

let decode =
  let rec interpret atom = function
    | Field -> fields long_form
    | Action -> short_form
    | Since (version, inner) ->
      let what = Printf.sprintf "'%s' in short-form 'rule'" atom in
      Dune_lang.Syntax.since ~what Stanza.syntax version >>> interpret atom inner
  in
  peek_exn
  >>= function
  | List (_, Atom (loc, A s) :: _) ->
    (match String.Map.find atom_table s with
     | None ->
       User_error.raise
         ~loc
         [ Pp.text "Unknown action or rule field." ]
         ~hints:(User_message.did_you_mean s ~candidates:(String.Map.keys atom_table))
     | Some w -> interpret s w)
  | sexp ->
    User_error.raise
      ~loc:(Dune_lang.Ast.loc sexp)
      [ Pp.textf "S-expression of the form (<atom> ...) expected" ]
;;

type lex_or_yacc =
  { modules : string list
  ; mode : Rule.Mode.t
  ; enabled_if : Blang.t
  }

let ocamllex =
  (let+ modules = repeat string in
   { modules; mode = Standard; enabled_if = Blang.true_ })
  <|> fields
        (let+ modules = field "modules" (repeat string)
         and+ mode = Mode.field
         and+ enabled_if = Enabled_if.decode ~allowed_vars:Any ~since:(Some (1, 4)) () in
         { modules; mode; enabled_if })
;;

let ocamlyacc = ocamllex

let ocamllex_to_rule loc { modules; mode; enabled_if } =
  let module S = String_with_vars in
  List.map modules ~f:(fun name ->
    let src = name ^ ".mll" in
    let dst = name ^ ".ml" in
    let dst_s = S.make_text loc dst in
    { targets =
        Static
          { targets = [ (dst_s, File) ]
          ; multiplicity = Multiple
          ; named_targets = [ "main", dst_s ]
          }
    ; deps = Bindings.singleton (Dep_conf.File (S.virt_text __POS__ src))
    ; action =
        ( loc
        , Chdir
            ( S.virt_pform __POS__ (Var Workspace_root)
            , Dune_lang.Action.run
                (S.virt_text __POS__ "ocamllex")
                [ S.virt_text __POS__ "-q"
                ; S.virt_text __POS__ "-o"
                ; S.virt_text __POS__ dst  
                ; S.virt_pform __POS__ (Var Deps)
                ] ) )
    ; mode
    ; locks = []
    ; loc
    ; enabled_if
    ; aliases = []
    ; package = None
    }
  )
;;

let ocamlyacc_to_rule loc { modules; mode; enabled_if } =
  let module S = String_with_vars in
  List.map modules ~f:(fun name ->
    let src = name ^ ".mly" in
    { targets =
        Static
        { targets =
        List.map
          [ name ^ ".ml"; name ^ ".mli" ]
          ~f:(fun target -> 
            let target_s = S.make_text loc target in
            (target_s, Base_targets_spec.Kind.File))
          ; multiplicity = Multiple
          ; named_targets =
              [ "implementation", S.make_text loc (name ^ ".ml")
              ; "interface", S.make_text loc (name ^ ".mli")
              ]
          }
    ; deps = Bindings.singleton (Dep_conf.File (S.virt_text __POS__ src))
    ; action =
        ( loc
        , Chdir
            ( S.virt_pform __POS__ (Var Workspace_root)
            , Dune_lang.Action.run
                (S.virt_text __POS__ "ocamlyacc")
                [ S.virt_pform __POS__ (Var Deps) ] ) )
    ; mode
    ; locks = []
    ; loc
    ; enabled_if
    ; aliases = []
    ; package = None
    })
;;

let decode_named_target =
  let open Dune_lang.Decoder in
  let+ loc = loc
  and+ name = string
  and+ target = String_with_vars.decode in
  loc, (name, target)
;;

let decode_targets =
  let* project = Dune_project.get_exn () in
  let _allow_directory_targets =
    Dune_project.is_extension_set project directory_targets_extension
  in
  let open Dune_lang.Decoder in
  (* Create the targets parser *)
  let targets_parser = repeat (String_with_vars.decode >>| fun x -> x, Base_targets_spec.Kind.File) in
  
  (* Create a complete fields parser with both target fields *)
  fields (
    let+ targets = field "targets" targets_parser ~default:[]
    and+ named_targets = field "named_targets" (repeat decode_named_target) ~default:[] in
    let named_targets = List.map named_targets ~f:(fun (_, pair) -> pair) in
    Base_targets_spec.Static { targets; multiplicity = Multiple; named_targets }
  )
;;