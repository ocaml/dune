open Import
open Decoder

module Pps_and_flags = struct
  let decode =
    let+ loc = loc
    and+ l, flags =
      until_keyword
        "--"
        ~before:String_with_vars.decode
        ~after:(repeat String_with_vars.decode)
    and+ syntax_version = Syntax.get_exn Stanza.syntax in
    let pps, more_flags =
      List.partition_map l ~f:(fun s ->
        match
          ( String_with_vars.is_prefix ~prefix:"-" s
          , String_with_vars.is_prefix ~prefix:"+" s )
        with
        | Yes, _ -> Right s
        | _, Yes ->
          if syntax_version >= (3, 18)
          then Right s
          else
            User_error.raise
              ~loc:(String_with_vars.loc s)
              ~hints:[ Pp.text "Upgrade your dune-project to `(lang dune 3.18)'" ]
              [ Pp.text
                  "PPX args starting with `+' cannot be used before version 3.18 of the \
                   dune language"
              ]
        | (No | Unknown _), _ ->
          let loc = String_with_vars.loc s in
          (match String_with_vars.text_only s with
           | None ->
             User_error.raise ~loc [ Pp.text "No variables allowed in ppx library names" ]
           | Some txt -> Left (loc, Lib_name.parse_string_exn (loc, txt))))
    in
    let all_flags = more_flags @ Option.value flags ~default:[] in
    if syntax_version < (1, 10)
    then
      List.iter
        ~f:(fun flag ->
          if String_with_vars.has_pforms flag
          then
            Syntax.Error.since
              (String_with_vars.loc flag)
              Stanza.syntax
              (1, 10)
              ~what:"Using variables in pps flags")
        all_flags;
    if pps = []
    then User_error.raise ~loc [ Pp.text "You must specify at least one ppx rewriter." ];
    pps, all_flags
  ;;
end

module Pps = struct
  type 'a t =
    { loc : Loc.t
    ; pps : 'a list
    ; flags : String_with_vars.t list
    ; staged : bool
    }

  let equal f t { loc; pps; flags; staged } =
    Loc.equal t.loc loc
    && List.equal f t.pps pps
    && List.equal String_with_vars.equal_no_loc t.flags flags
    && Bool.equal t.staged staged
  ;;

  let compare_no_locs compare_pps { pps; flags; staged; loc = _ } t =
    let open Ordering.O in
    let= () = Bool.compare staged t.staged in
    let= () = List.compare flags t.flags ~compare:String_with_vars.compare_no_loc in
    List.compare pps t.pps ~compare:compare_pps
  ;;
end

type 'a t =
  | No_preprocessing
  | Action of Loc.t * Action.t
  | Pps of 'a Pps.t
  | Future_syntax of Loc.t

let equal f x y =
  match x, y with
  | No_preprocessing, No_preprocessing -> true
  | Action (x, y), Action (x', y') -> Tuple.T2.equal Loc.equal Action.equal (x, y) (x', y')
  | Pps x, Pps y -> Pps.equal f x y
  | Future_syntax x, Future_syntax y -> Loc.equal x y
  | _, _ -> false
;;

let map t ~f =
  match t with
  | Pps t -> Pps { t with pps = List.map t.pps ~f }
  | (No_preprocessing | Action _ | Future_syntax _) as t -> t
;;

let filter_map t ~f =
  match t with
  | Pps t ->
    let pps = List.filter_map t.pps ~f in
    if pps = [] then No_preprocessing else Pps { t with pps }
  | (No_preprocessing | Action _ | Future_syntax _) as t -> t
;;

module Without_instrumentation = struct
  type t = Loc.t * Lib_name.t

  let compare_no_locs (_, x) (_, y) = Lib_name.compare x y
end

module With_instrumentation = struct
  type t =
    | Ordinary of Without_instrumentation.t
    | Instrumentation_backend of
        { libname : Loc.t * Lib_name.t
        ; deps : Dep_conf.t list
        ; flags : String_with_vars.t list
        }

  let equal (x : t) (y : t) = Poly.equal x y
end

let decode =
  sum
    [ "no_preprocessing", return No_preprocessing
    ; ( "action"
      , let+ loc, x =
          located
            (update_var
               String_with_vars.decoding_env_key
               ~f:(fun env ->
                 let env = Option.value_exn env in
                 Some (Pform.Env.lt_renamed_input_file env))
               Action.decode_dune_file)
        in
        Action (loc, x) )
    ; ( "pps"
      , let+ loc = loc
        and+ pps, flags = Pps_and_flags.decode in
        Pps { loc; pps; flags; staged = false } )
    ; ( "staged_pps"
      , let+ () = Syntax.since Stanza.syntax (1, 1)
        and+ loc = loc
        and+ pps, flags = Pps_and_flags.decode in
        Pps { loc; pps; flags; staged = true } )
    ; ( "future_syntax"
      , let+ () = Syntax.since Stanza.syntax (1, 8)
        and+ loc = loc in
        Future_syntax loc )
    ]
;;

let loc = function
  | No_preprocessing -> None
  | Action (loc, _) | Pps { loc; _ } | Future_syntax loc -> Some loc
;;

let pps = function
  | Pps { pps; _ } -> pps
  | _ -> []
;;

module Without_future_syntax = struct
  type 'a t =
    | No_preprocessing
    | Action of Loc.t * Action.t
    | Pps of 'a Pps.t
end

module Pp_flag_consumer = struct
  (* Compiler allows the output of [-pp] to be a binary AST. Merlin requires
     that to be a text file instead. *)
  type t =
    | Compiler
    | Merlin
end

let remove_future_syntax (t : 'a t) ~(for_ : Pp_flag_consumer.t) v
  : 'a Without_future_syntax.t
  =
  match t with
  | No_preprocessing -> No_preprocessing
  | Action (loc, action) -> Action (loc, action)
  | Pps pps -> Pps pps
  | Future_syntax loc ->
    if Ocaml.Version.supports_let_syntax v
    then No_preprocessing
    else
      Action
        ( loc
        , Action.run
            (String_with_vars.make_pform
               loc
               (Macro
                  { Pform.Macro_invocation.macro = Bin
                  ; payload = Pform.Payload.of_args [ "ocaml-syntax-shims" ]
                  }))
            ((match for_ with
              | Compiler -> [ String_with_vars.make_text loc "-dump-ast" ]
              | Merlin ->
                (* We generate a text file instead of AST. That gives you less
                   precise locations, but at least Merlin doesn't fail outright.

                   In general this hack should be applied to all -pp commands
                   that might produce an AST, not just to Future_syntax. But
                   doing so means we need to change dune language so the user
                   can provide two versions of the command.

                   Hopefully this will be fixed in merlin before that becomes a
                   necessity. *)
                [])
             @ [ String_with_vars.make_pform loc (Var Input_file) ]) )
;;

module Instrumentation = struct
  type t =
    { backend : Loc.t * Lib_name.t
    ; flags : String_with_vars.t list
    ; deps : Dep_conf.t list
    ; loc : Loc.t
    }

  let instrumentation =
    multi_field
      "instrumentation"
      (Syntax.since Stanza.syntax (2, 7)
       >>> fields
           @@
           let+ backend, flags =
             field "backend"
             @@ let+ libname = located Lib_name.decode
                and+ flags =
                  let* current_ver = Syntax.get_exn Stanza.syntax in
                  let version_check flag =
                    let ver = 2, 8 in
                    if current_ver >= ver
                    then flag
                    else (
                      let what =
                        "The possibility to pass arguments to instrumentation backends"
                      in
                      Syntax.Error.since
                        (String_with_vars.loc flag)
                        Stanza.syntax
                        ver
                        ~what)
                  in
                  repeat (String_with_vars.decode >>| version_check)
                in
                libname, flags
           and+ deps =
             field
               "deps"
               ~default:[]
               (Syntax.since Stanza.syntax (2, 9) >>> repeat Dep_conf.decode)
           and+ loc = Decoder.loc in
           { backend; deps; flags; loc })
  ;;
end

module Per_module = struct
  module Per_module = Module_name.Per_item

  type 'a preprocess = 'a t
  type 'a t = 'a preprocess Per_module.t

  let equal f x y = Per_module.equal (equal f) x y
  let decode = Per_module.decode decode ~default:No_preprocessing
  let no_preprocessing () = Per_module.for_all No_preprocessing
  let find module_name t = Per_module.get t module_name
  let default () = Per_module.for_all No_preprocessing

  let pps t =
    Per_module.fold t ~init:Lib_name.Map.empty ~f:(fun pp acc ->
      List.fold_left (pps pp) ~init:acc ~f:(fun acc (loc, pp) ->
        Lib_name.Map.set acc pp loc))
    |> Lib_name.Map.foldi ~init:[] ~f:(fun pp loc acc -> (loc, pp) :: acc)
  ;;

  let add_instrumentation t { Instrumentation.flags; loc; deps; backend = libname } =
    Per_module.map t ~f:(fun pp ->
      match pp with
      | No_preprocessing ->
        let pps =
          [ With_instrumentation.Instrumentation_backend { libname; deps; flags } ]
        in
        Pps { loc; pps; flags = []; staged = false }
      | Pps ({ pps; _ } as t) ->
        (* CR-rgrinberg: Maybe we shouldn't drop the loc of the instrumentation
           stanza? *)
        let pps =
          With_instrumentation.Instrumentation_backend { libname; deps; flags } :: pps
        in
        Pps { t with pps }
      | Action (loc, _) | Future_syntax loc ->
        User_error.raise
          ~loc
          [ Pp.text
              "Preprocessing with actions and future syntax cannot be used in \
               conjunction with (instrumentation ...)"
          ])
  ;;

  let without_instrumentation t =
    let f = function
      | With_instrumentation.Ordinary libname -> Some libname
      | Instrumentation_backend _ -> None
    in
    Per_module.map t ~f:(filter_map ~f)
  ;;
end

let preprocess_fields =
  let open Decoder in
  let+ preprocess = field "preprocess" Per_module.decode ~default:(Per_module.default ())
  and+ preprocessor_deps =
    field_o
      "preprocessor_deps"
      (let+ loc = loc
       and+ l = repeat Dep_conf.decode in
       loc, l)
  and+ syntax = Syntax.get_exn Stanza.syntax in
  let preprocessor_deps =
    match preprocessor_deps with
    | None -> []
    | Some (loc, deps) ->
      let deps_might_be_used =
        Module_name.Per_item.exists preprocess ~f:(fun p ->
          match p with
          | Action _ | Pps _ -> true
          | No_preprocessing | Future_syntax _ -> false)
      in
      if not deps_might_be_used
      then
        User_warning.emit
          ~loc
          ~is_error:(syntax >= (2, 0))
          [ Pp.text
              "This preprocessor_deps field will be ignored because no preprocessor that \
               might use them is configured."
          ];
      deps
  in
  preprocess, preprocessor_deps
;;
