open! Stdune
open Import
open Build.O
open Dune_file

module SC = Super_context

(* Encoded representation of a set of library names + scope *)
module Key : sig
  (* This module implements a bi-directional function between
     [encoded] and [decoded] *)
  type encoded = Digest.t
  type decoded =
    { pps   : Lib_name.t list
    ; scope : Dune_project.Name.t option
    }

  val of_libs : dir_kind:Dune_lang.File_syntax.t -> Lib.t list -> decoded

  (* [decode y] fails if there hasn't been a previous call to [encode]
     such that [encode x = y]. *)
  val encode : decoded -> encoded
  val decode : encoded -> decoded
end = struct
  type encoded = Digest.t
  type decoded =
    { pps   : Lib_name.t list
    ; scope : Dune_project.Name.t option
    }

  let reverse_table : (encoded, decoded) Hashtbl.t = Hashtbl.create 128

  let of_libs ~dir_kind libs =
    let pps =
      (let compare a b = Lib_name.compare (Lib.name a) (Lib.name b) in
       match (dir_kind : Dune_lang.File_syntax.t) with
       | Dune -> List.sort libs ~compare
       | Jbuild ->
         match List.rev libs with
         | last :: others -> List.sort others ~compare @ [last]
         | [] -> [])
      |> List.map ~f:Lib.name
    in
    let scope =
      List.fold_left libs ~init:None ~f:(fun acc lib ->
        let scope_for_key =
          match Lib.status lib with
          | Private scope_name   -> Some scope_name
          | Public _ | Installed -> None
        in
        let open Dune_project.Name.Infix in
        match acc, scope_for_key with
        | Some a, Some b -> assert (a = b); acc
        | Some _, None   -> acc
        | None  , Some _ -> scope_for_key
        | None  , None   -> None)
    in
    { pps; scope }

  let encode x =
    let y = Digest.string (Marshal.to_string x []) in
    match Hashtbl.find reverse_table y with
    | None ->
      Hashtbl.add reverse_table y x;
      y
    | Some x' ->
      if x = x' then
        y
      else begin
        let to_string { pps; scope } =
          let s = String.enumerate_and (List.map pps ~f:Lib_name.to_string) in
          match scope with
          | None -> s
          | Some scope ->
            sprintf "%s (in project: %s)" s
              (Dune_project.Name.to_string_hum scope)
        in
        User_error.raise [ Pp.textf "Hash collision between set of ppx drivers:\n\
             - cache : %s\n\
             - fetch : %s"
          (to_string x')
          (to_string x) ]
      end

  let decode y =
    match Hashtbl.find reverse_table y with
    | Some x -> x
    | None ->
      User_error.raise [ Pp.textf "I don't know what ppx rewriters set %s correspond to."
        (Digest.to_string y) ]
end

let pped_module m ~f =
  let pped = Module.pped m in
  Module.iter m ~f:(fun kind file ->
    let pp_path =
      Module.file pped kind
      |> Option.value_exn
      |> Path.as_in_build_dir_exn
    in
    let file = Path.as_in_build_dir_exn file.path in
    f kind file pp_path);
  pped

module Driver = struct
  module M = struct
    module Info = struct
      let name = Sub_system_name.make "ppx.driver"
      type t =
        { loc          : Loc.t
        ; flags        : Ordered_set_lang.Unexpanded.t
        ; as_ppx_flags : Ordered_set_lang.Unexpanded.t
        ; lint_flags   : Ordered_set_lang.Unexpanded.t
        ; main         : string
        ; replaces     : (Loc.t * Lib_name.t) list
        ; file_kind    : Stanza.File_kind.t
        }

      type Sub_system_info.t += T of t

      let loc t = t.loc

      (* The syntax of the driver sub-system is part of the main dune
         syntax, so we simply don't create a new one.

         If we wanted to make the ppx system an extension, then we
         would create a new one.
      *)
      let syntax = Stanza.syntax

      open Stanza.Decoder

      let parse =
        record
          (let+ loc = loc
           and+ flags = Ordered_set_lang.Unexpanded.field "flags"
           and+ as_ppx_flags =
             Ordered_set_lang.Unexpanded.field "as_ppx_flags"
               ~check:(Syntax.since syntax (1, 2))
               ~default:(Ordered_set_lang.Unexpanded.of_strings ["--as-ppx"]
                           ~pos:__POS__)
           and+ lint_flags = Ordered_set_lang.Unexpanded.field "lint_flags"
           and+ main = field "main" string
           and+ replaces =
             field "replaces" (list (located (Lib_name.decode))) ~default:[]
           and+ file_kind = Stanza.file_kind ()
           in
           { loc
           ; flags
           ; as_ppx_flags
           ; lint_flags
           ; main
           ; replaces
           ; file_kind
           })
    end

    (* The [lib] field is lazy so that we don't need to fill it for
       hardcoded [t] values used to implement the jbuild style
       handling of drivers.

       See [Jbuild_driver] below for details. *)
    type t =
      { info     : Info.t
      ; lib      : Lib.t Lazy.t
      ; replaces : t list Or_exn.t
      }

    let desc ~plural = "ppx driver" ^ if plural then "s" else ""
    let desc_article = "a"

    let lib      t = Lazy.force t.lib
    let replaces t = t.replaces

    let instantiate ~resolve ~get lib (info : Info.t) =
      { info
      ; lib = lazy lib
      ; replaces =
          let open Result.O in
          Result.List.map info.replaces ~f:(fun ((loc, name) as x) ->
            let* lib = resolve x in
            match get ~loc lib with
            | None ->
              Error (Errors.exnf loc "%a is not a %s"
                       Lib_name.pp_quoted name
                       (desc ~plural:false))
            | Some t -> Ok t)
      }

    let encode t =
      let open Dune_lang.Encoder in
      let f x = Lib_name.encode (Lib.name (Lazy.force x.lib)) in
      ((1, 0),
       record_fields @@
         [ field_i "flags" Ordered_set_lang.Unexpanded.encode_and_upgrade
             t.info.flags
         ; field_i "lint_flags" Ordered_set_lang.Unexpanded.encode_and_upgrade
             t.info.lint_flags
         ; field "main" string t.info.main
         ; field_l "replaces" f (Result.ok_exn t.replaces)
         ])
  end
  include M
  include Sub_system.Register_backend(M)

  (* Where are we called from? *)
  type loc =
    | User_file of Loc.t * (Loc.t * Lib_name.t) list
    | Dot_ppx   of Path.Build.t * Lib_name.t list

  let make_error loc msg =
    match loc with
    | User_file (loc, _) -> Error (Errors.exnf loc "%a" Fmt.text msg)
    | Dot_ppx (path, pps) ->
      Error (Errors.exnf (Loc.in_file (Path.build path)) "%a" Fmt.text
               (sprintf
                  "Failed to create on-demand ppx rewriter for %s; %s"
                  (String.enumerate_and (List.map pps ~f:Lib_name.to_string))
                  (String.uncapitalize msg)))

  let select libs ~loc =
    match select_replaceable_backend libs ~replaces with
    | Ok _ as x -> x
    | Error No_backend_found ->
      let msg =
        match libs with
        | [] ->
          "You must specify at least one ppx rewriter."
        | _ ->
          match
            List.filter_map libs ~f:(fun lib ->
              match Lib_name.to_string (Lib.name lib) with
              | "ocaml-migrate-parsetree" | "ppxlib" | "ppx_driver" as s ->
                Some s
              | _ -> None)
          with
          | [] ->
            let pps =
              match loc with
              | User_file (_, pps) -> List.map pps ~f:snd
              | Dot_ppx (_, pps) -> pps
            in
            sprintf
              "No ppx driver were found. It seems that %s %s not \
               compatible with Dune. Examples of ppx rewriters that \
               are compatible with Dune are ones using \
               ocaml-migrate-parsetree, ppxlib or ppx_driver."
              (String.enumerate_and (List.map pps ~f:Lib_name.to_string))
              (match pps with
               | [_] -> "is"
               | _   -> "are")
          | names ->
            sprintf
              "No ppx driver were found.\n\
               Hint: Try upgrading or reinstalling %s."
              (String.enumerate_and names)
      in
      make_error loc msg
    | Error (Too_many_backends ts) ->
      make_error loc
        (sprintf
           "Too many incompatible ppx drivers were found: %s."
           (String.enumerate_and (List.map ts ~f:(fun t ->
              Lib_name.to_string (Lib.name (lib t))))))
    | Error (Other exn) ->
      Error exn
end

module Jbuild_driver = struct
  (* This module is used to implement the jbuild handling of ppx
     drivers.  It doesn't implement exactly the same algorithm, but it
     should be enough for all jbuilder packages out there.

     It works as follow: given the list of ppx rewriters specified by
     the user, check whether the last one is named [ppxlib.runner] or
     [ppx_driver.runner]. If it isn't, assume the driver is
     ocaml-migrate-parsetree and use some hard-coded driver
     information. If it is, use the corresponding hardcoded driver
     information. *)

  let make name info : (Lib_name.t * Driver.t) Lazy.t = lazy (
    let info =
      let parsing_context =
        Univ_map.singleton (Syntax.key Stanza.syntax) (0, 0)
      in
      let fname = Printf.sprintf "<internal-%s>" name in
      Dune_lang.parse_string ~mode:Single ~fname info
        ~lexer:Dune_lang.Lexer.jbuild_token
      |> Dune_lang.Decoder.parse Driver.Info.parse parsing_context
    in
    (Lib_name.of_string_exn ~loc:None name,
     { info
     ; lib = lazy (assert false)
     ; replaces = Ok []
     }))
  let omp = make "ocaml-migrate-parsetree" {|
    ((main       Migrate_parsetree.Driver.run_main)
     (flags      (--dump-ast))
     (lint_flags (--null)))
  |}
  let ppxlib = make "ppxlib" {|
    ((main       Ppxlib.Driver.standalone)
     (flags      (-diff-cmd - -dump-ast))
     (lint_flags (-diff-cmd - -null    )))
  |}
  let ppx_driver = make "ppx_driver" {|
    ((main       Ppx_driver.standalone)
     (flags      (-diff-cmd - -dump-ast))
     (lint_flags (-diff-cmd - -null    )))
  |}

  let drivers =
    let name = Lib_name.of_string_exn ~loc:None in
    [ name "ocaml-migrate-parsetree.driver-main" , omp
    ; name "ppxlib.runner"                       , ppxlib
    ; name "ppx_driver.runner"                   , ppx_driver
    ]

  let get_driver pps =
    let driver =
      match List.last pps with
      | None -> omp
      | Some (_, pp) -> Option.value (List.assoc drivers pp) ~default:omp
    in
    snd (Lazy.force driver)

  (* For building the driver *)
  let analyse_pps pps ~get_name =
    let driver, rev_others =
      match List.rev pps with
      | [] -> (omp, [])
      | pp :: rev_rest as rev_pps ->
        match List.assoc drivers (get_name pp) with
        | None        -> (omp   , rev_pps )
        | Some driver -> (driver, rev_rest)
    in
    let driver_name, driver = Lazy.force driver in
    (driver, driver_name, rev_others)
end

let ppx_exe sctx ~key ~dir_kind =
  match (dir_kind : Dune_lang.File_syntax.t) with
  | Dune ->
    Path.Build.relative (SC.build_dir sctx) (".ppx/" ^ key ^ "/ppx.exe")
  | Jbuild ->
    Path.Build.relative (SC.build_dir sctx) (".ppx/jbuild/" ^ key ^ "/ppx.exe")

let build_ppx_driver sctx ~dep_kind ~target ~dir_kind ~pps ~pp_names =
  let ctx = SC.context sctx in
  let mode = Context.best_mode ctx in
  let compiler = Option.value_exn (Context.compiler ctx mode) in
  let jbuild_driver, pps, pp_names =
    match (dir_kind : Dune_lang.File_syntax.t) with
    | Dune -> (None, pps, pp_names)
    | Jbuild ->
      match pps with
      | Error _ ->
        let driver, driver_name, pp_names =
          Jbuild_driver.analyse_pps pp_names ~get_name:Fn.id
        in
        (Some driver, pps, driver_name :: pp_names)
      | Ok pps ->
        let driver, driver_name, pps =
          Jbuild_driver.analyse_pps pps ~get_name:Lib.name
        in
        let pp_names = driver_name :: List.map pps ~f:Lib.name in
        let pps =
          let open Result.O in
          let+ driver =
            Lib.DB.resolve_pps (SC.public_libs sctx) [(Loc.none, driver_name)]
          in
          driver @ pps
        in
        (Some driver, pps, pp_names)
  in
  let driver_and_libs =
    let open Result.O in
    Result.map_error ~f:(fun e ->
      (* Extend the dependency stack as we don't have locations at
         this point *)
      Dep_path.prepend_exn e (Preprocess pp_names))
      (let* pps = pps in
       let* pps = Lib.closure ~linking:true pps in
       match jbuild_driver with
       | None ->
         let+ driver =
           Driver.select pps ~loc:(Dot_ppx (target, pp_names))
         in
         (driver, pps)
       | Some driver ->
         Ok (driver, pps))
  in
  (* CR-someday diml: what we should do is build the .cmx/.cmo once
     and for all at the point where the driver is defined. *)
  let dir = Path.Build.parent_exn target in
  let ml = Path.Build.relative dir "_ppx.ml" in
  let add_rule = SC.add_rule sctx ~dir in
  add_rule
    (Build.of_result_map driver_and_libs ~f:(fun (driver, _) ->
       Build.return (sprintf "let () = %s ()\n" driver.info.main))
     >>>
     Build.write_file_dyn ml);
  add_rule
    (Build.S.seqs
       [Build.record_lib_deps
          (Lib_deps.info ~kind:dep_kind (Lib_deps.of_pps pp_names));
        Build.of_result_map driver_and_libs ~f:(fun (_, libs) ->
          Build.paths (Lib.L.archive_files libs ~mode))]
       (Command.run (Ok compiler) ~dir:(Path.build ctx.build_dir)
          [ A "-o" ; Target target
          ; A "-w"; A "-24"
          ; Command.of_result
              (Result.map driver_and_libs ~f:(fun (_driver, libs) ->
                 Lib.L.compile_and_link_flags ~mode ~stdlib_dir:ctx.stdlib_dir
                   ~compile:libs
                   ~link:libs))
          ; Dep (Path.build ml)
          ]))

let get_rules sctx key ~dir_kind =
  let exe = ppx_exe sctx ~key ~dir_kind in
  let pps, pp_names =
    let names, lib_db =
      match Digest.from_hex key with
      | key ->
        let { Key.pps; scope } = Key.decode key in
        let lib_db =
          match scope with
          | None -> SC.public_libs sctx
          | Some name -> Scope.libs (SC.find_scope_by_name sctx name)
        in
        (pps, lib_db)
      | exception _ ->
        (* Still support the old scheme for backward compatibility *)
        let (key, lib_db) = SC.Scope_key.of_string sctx key in
        let names =
          match key with
          | "+none+" -> []
          | _ -> String.split key ~on:'+'
        in
        let names =
          match List.rev names with
          | [] -> []
          | driver :: rest -> List.sort rest ~compare:String.compare @ [driver]
        in
        let names = List.map names ~f:(Lib_name.of_string_exn ~loc:None) in
        (names, lib_db)
    in
    let pps =
      Lib.DB.resolve_pps lib_db (List.map names ~f:(fun x -> (Loc.none, x)))
    in
    (pps, names)
  in
  build_ppx_driver sctx ~pps ~pp_names ~dep_kind:Required ~target:exe ~dir_kind

let gen_rules sctx components =
  match components with
  | [key] -> get_rules sctx key ~dir_kind:Dune
  | ["jbuild"; key] -> get_rules sctx key ~dir_kind:Jbuild
  | _ -> ()

let ppx_driver_exe sctx libs ~dir_kind =
  let key = Digest.to_string (Key.of_libs ~dir_kind libs |> Key.encode) in
  ppx_exe sctx ~key ~dir_kind

module Compat_ppx_exe_kind = struct
  type t =
    | Dune
    | Jbuild of string option
end

let get_compat_ppx_exe sctx ~name ~kind =
  let name = Lib_name.to_string name in
  match (kind : Compat_ppx_exe_kind.t) with
  | Dune ->
    ppx_exe sctx ~key:name ~dir_kind:Dune
  | Jbuild driver ->
    (* We know both [name] and [driver] are public libraries, so we
       don't add the scope key. *)
    let key =
      match driver with
      | None -> name
      | Some d -> sprintf "%s+%s" name d
    in
    ppx_exe sctx ~key ~dir_kind:Jbuild

let get_cookies ~loc ~expander ~lib_name libs =
  let expander, library_name_cookie =
    match lib_name with
    | None -> expander, None
    | Some lib_name ->
      let library_name = Lib_name.Local.to_string lib_name in
      let bindings =
        Pform.Map.singleton "library_name"
          (Values [String library_name])
      in
      Expander.add_bindings expander ~bindings,
      Some ("library-name", (library_name, Lib_name.of_local (loc, lib_name)))
  in
  try
    Ok (libs
      |> List.concat_map ~f:
        (fun t ->
          match Lib.kind t with
          | Normal -> []
          | Ppx_rewriter {cookies}
          | Ppx_deriver {cookies} ->
            List.map ~f:(fun {Lib_kind.Ppx_args.Cookie.name; value} ->
              (name, (Expander.expand_str expander value, Lib.name t)))
              cookies
        )
      |> (fun l ->
        match library_name_cookie with
        | None -> l
        | Some cookie -> cookie :: l
      )
      |> String.Map.of_list_reducei ~f:
        (fun name ((val1, lib1) as res) (val2, lib2) ->
          if String.equal val1 val2 then
            res
          else
            let lib1 = Lib_name.to_string lib1 in
            let lib2 = Lib_name.to_string lib2 in
            Errors.fail loc "%a" Fmt.text
             (sprintf "%s and %s have inconsistent requests for cookie %S; \
                       %s requests %S and %s requests %S"
                lib1 lib2 name
                lib1 val1
                lib2 val2)
        )
      |> String.Map.foldi ~init:[]
        ~f:(fun name (value, _) acc -> (name, value) :: acc)
      |> List.rev
      |> List.concat_map ~f:
        (fun (name, value) ->
          ["--cookie"; sprintf "%s=%S" name value]
        )
      )
  with exn -> Error exn

let ppx_driver_and_flags_internal sctx ~loc ~expander ~lib_name ~flags
      ~dir_kind libs =
  let open Result.O in
  let flags = List.map ~f:(Expander.expand_str expander) flags in
  let+ cookies = get_cookies ~loc ~lib_name ~expander libs in
  let sctx = SC.host sctx in
  ppx_driver_exe sctx libs ~dir_kind, flags @ cookies

let ppx_driver_and_flags sctx ~lib_name ~expander ~scope ~loc ~dir_kind ~flags
      pps =
  let open Result.O in
  let* libs = Lib.DB.resolve_pps (Scope.libs scope) pps in
  let* exe, flags = ppx_driver_and_flags_internal sctx ~loc ~expander ~lib_name
                      ~flags ~dir_kind libs in
  let+ driver =
    match (dir_kind : Dune_lang.File_syntax.t) with
    | Dune ->
      Lib.closure libs ~linking:true
      >>=
      Driver.select ~loc:(User_file (loc, pps))
    | Jbuild ->
      Ok (Jbuild_driver.get_driver pps)
  in
  (exe, driver, flags)


let workspace_root_var = String_with_vars.virt_var __POS__ "workspace_root"


(* Generate rules for the reason modules in [modules] and return a
   a new module with only OCaml sources *)
let setup_reason_rules sctx ~dir (m : Module.t) =
  let refmt = Refmt.get sctx ~loc:None ~dir in
  let rule input output = Refmt.to_ocaml_ast refmt ~input ~output in
  let ml = Module.ml_source m in
  Module.iter m ~f:(fun kind f ->
    match f.syntax with
    | OCaml  ->
      ()
    | Reason ->
      let ml =
        Option.value_exn (Module.file ml kind)
        |> Path.as_in_build_dir_exn
      in
      SC.add_rule sctx ~dir (rule f.path ml));
  ml

let promote_correction fn build ~suffix =
  Build.progn
    [ build
    ; Build.return
        (Action.diff ~optional:true
           fn
           (Path.extend_basename fn ~suffix))
    ]

let chdir action = Action_unexpanded.Chdir (workspace_root_var, action)

let action_for_pp sctx ~dep_kind ~loc ~expander ~action ~src ~target =
  let action = chdir action in
  let bindings = Pform.Map.input_file (Path.build src) in
  let expander = Expander.add_bindings expander ~bindings in
  let targets = Expander.Targets.Forbidden "preprocessing actions" in
  let targets_dir =
    Option.value ~default:src target
    |> Path.Build.parent_exn
  in
  Build.path (Path.build src)
  >>^ (fun _ -> Bindings.empty)
  >>>
  SC.Action.run sctx
    action
    ~loc
    ~expander
    ~dep_kind
    ~targets
    ~targets_dir
  |> (fun action ->
    match target with
    | None -> action
    | Some dst -> action >>> Build.action_dyn () ~targets:[dst])
  >>^ fun action ->
  match target with
  | None -> action
  | Some dst -> Action.with_stdout_to (Path.build dst) action

let lint_module sctx ~dir ~expander ~dep_kind ~lint ~lib_name ~scope ~dir_kind =
  Staged.stage (
    let alias = Alias.lint ~dir in
    let add_alias fn build =
      SC.add_alias_action sctx alias build ~dir
        ~stamp:("lint", lib_name, fn)
    in
    let lint =
      Per_module.map lint ~f:(function
        | Preprocess.No_preprocessing ->
          (fun ~source:_ ~ast:_ -> ())
        | Future_syntax loc ->
          Errors.fail loc
            "'compat' cannot be used as a linter"
        | Action (loc, action) ->
          (fun ~source ~ast:_ ->
             Module.iter source ~f:(fun _ (src : Module.File.t) ->
               let src = Path.as_in_build_dir_exn src.path in
               add_alias src ~loc:(Some loc)
                 (action_for_pp sctx ~dep_kind ~loc ~expander ~action
                    ~src ~target:None)))
        | Pps { loc; pps; flags; staged } ->
          if staged then
            Errors.fail loc
              "Staged ppx rewriters cannot be used as linters.";
          let corrected_suffix = ".lint-corrected" in
          let driver_and_flags =
            let open Result.O in
            let+ (exe, driver, driver_flags) =
              ppx_driver_and_flags sctx ~expander ~loc ~lib_name ~flags
                ~dir_kind ~scope pps
            in
            let flags =
              let bindings =
                Pform.Map.singleton "corrected-suffix"
                  (Values [String corrected_suffix])
              in
              let expander = Expander.add_bindings expander ~bindings in
              Build.memoize "ppx flags"
                (Expander.expand_and_eval_set expander driver.info.lint_flags
                   ~standard:(Build.return []))
            in
            let args : _ Command.Args.t = S [ As driver_flags ] in
            (exe, flags, args)
          in
          (fun ~source ~ast ->
             Module.iter ast ~f:(fun kind src ->
               add_alias src.path
                 ~loc:None
                 (promote_correction ~suffix:corrected_suffix
                    (Option.value_exn (Module.file source kind))
                    (Build.of_result_map driver_and_flags
                       ~f:(fun (exe, flags, args) ->
                         Command.run ~dir:(Path.build (SC.build_dir sctx))
                           (Ok (Path.build exe))
                           [ args
                           ; Ml_kind.ppx_driver_flag kind
                           ; Dep src.path
                           ; Command.Args.dyn flags
                           ]))))))
    in
    fun ~(source : Module.t) ~ast ->
      Per_module.get lint (Module.name source) ~source ~ast)

type t = (Module.t -> lint:bool -> Module.t) Per_module.t

let dummy = Per_module.for_all (fun m ~lint:_ -> m)

let make sctx ~dir ~expander ~dep_kind ~lint ~preprocess
      ~preprocessor_deps ~lib_name ~scope ~dir_kind =
  let preprocessor_deps =
    Build.memoize "preprocessor deps" preprocessor_deps
  in
  let lint_module =
    Staged.unstage (lint_module sctx ~dir ~expander ~dep_kind
                      ~lint ~lib_name ~scope ~dir_kind)
  in
  Per_module.map preprocess ~f:(fun pp ->
    match Dune_file.Preprocess.remove_future_syntax
            ~for_:Compiler
            pp
            (Super_context.context sctx).version
    with
    | No_preprocessing ->
      (fun m ~lint ->
         let ast = setup_reason_rules sctx ~dir m in
         if lint then lint_module ~ast ~source:m;
         ast)
    | Action (loc, action) ->
      (fun m ~lint ->
         let ast =
           pped_module m ~f:(fun _kind src dst ->
             let action = action_for_pp sctx ~dep_kind ~loc ~expander
                            ~action ~src ~target:(Some dst)
             in
             SC.add_rule sctx ~loc ~dir
               (preprocessor_deps >>> action))
           |> setup_reason_rules sctx ~dir
         in
         if lint then lint_module ~ast ~source:m;
         ast)
    | Pps { loc; pps; flags; staged } ->
      if not staged then begin
        let corrected_suffix = ".ppx-corrected" in
        let driver_and_flags =
          let open Result.O in
          let+ (exe, driver, flags) = ppx_driver_and_flags sctx ~expander ~loc
                                        ~lib_name ~flags ~dir_kind ~scope pps in
          let args : _ Command.Args.t = S [ As flags ] in
          (exe,
           (let bindings =
              Pform.Map.singleton "corrected-suffix"
                (Values [String corrected_suffix])
            in
            let expander = Expander.add_bindings expander ~bindings in
            Build.memoize "ppx flags"
              (Expander.expand_and_eval_set expander driver.info.flags
                 ~standard:(Build.return ["--as-ppx"]))), args)
        in
        (fun m ~lint ->
           let ast = setup_reason_rules sctx ~dir m in
           if lint then lint_module ~ast ~source:m;
           pped_module ast ~f:(fun kind src dst ->
             SC.add_rule sctx ~loc ~dir
               (promote_correction ~suffix:corrected_suffix
                  (Option.value_exn (Module.file m kind))
                  (preprocessor_deps >>^ ignore
                   >>>
                   Build.of_result_map driver_and_flags
                     ~targets:[dst]
                     ~f:(fun (exe, flags, args) ->
                       Command.run ~dir:(Path.build (SC.build_dir sctx))
                         (Ok (Path.build exe))
                         [ args
                         ; A "-o"; Target dst
                         ; Ml_kind.ppx_driver_flag kind; Dep (Path.build src)
                         ; Command.Args.dyn flags
                         ])))))
      end else begin
        let pp_flags = Build.of_result (
          let open Result.O in
          let+ (exe, driver, flags) =
            ppx_driver_and_flags sctx ~expander ~loc ~scope ~dir_kind ~flags
              ~lib_name pps
          in
          Build.memoize "ppx command"
            (Build.path (Path.build exe)
             >>>
             preprocessor_deps >>^ ignore
             >>>
             Expander.expand_and_eval_set expander driver.info.as_ppx_flags
               ~standard:(Build.return [])
             >>^ fun driver_flags ->
             let command =
               List.map
                 (List.concat
                    [ [Path.reach (Path.build exe)
                         ~from:(Path.build (SC.build_dir sctx))
                      ]
                    ; driver_flags
                    ; flags
                    ])
                 ~f:quote_for_shell
               |> String.concat ~sep:" "
             in
             ["-ppx"; command]))
        in
        let pp = Some pp_flags in
        (fun m ~lint ->
           let ast = setup_reason_rules sctx ~dir m in
           if lint then lint_module ~ast ~source:m;
           Module.set_pp ast pp)
      end)

let pp_modules t ?(lint=true) modules =
  Module.Name.Map.map modules ~f:(fun (m : Module.t) ->
    Per_module.get t (Module.name m) m ~lint)

let pp_module_as t ?(lint=true) name m =
  Per_module.get t name m ~lint

let get_ppx_driver sctx ~loc ~expander ~scope ~lib_name ~flags ~dir_kind pps =
  let open Result.O in
  let* libs = Lib.DB.resolve_pps (Scope.libs scope) pps in
  ppx_driver_and_flags_internal sctx ~loc ~expander ~lib_name ~flags ~dir_kind
    libs
