open Import
open Memo.O

(* Encoded representation of a set of library names + scope *)
module Key : sig
  (* This module implements a bi-directional function between [encoded] and
     [decoded] *)
  type encoded = Digest.t

  module Decoded : sig
    type t = private
      { pps : Lib_name.t list
      ; project_root : Path.Source.t option
      }

    val of_libs : Lib.t list -> t
  end

  (* [decode y] fails if there hasn't been a previous call to [encode] such that
     [encode x = y]. *)
  val encode : Decoded.t -> encoded

  val decode : encoded -> Decoded.t
end = struct
  type encoded = Digest.t

  module Decoded = struct
    (* Values of type type are preserved in a global table between builds, so
       they must not embed values that are not safe to keep between builds, such
       as [Dune_project.t] values *)
    type t =
      { pps : Lib_name.t list
      ; project_root : Path.Source.t option
      }

    let equal x y =
      List.equal Lib_name.equal x.pps y.pps
      && Option.equal Path.Source.equal x.project_root y.project_root

    let to_string { pps; project_root } =
      let s = String.enumerate_and (List.map pps ~f:Lib_name.to_string) in
      match project_root with
      | None -> s
      | Some dir ->
        sprintf "%s (in project: %s)" s (Path.Source.to_string_maybe_quoted dir)

    let of_libs libs =
      let pps =
        (let compare a b = Lib_name.compare (Lib.name a) (Lib.name b) in
         List.sort libs ~compare)
        |> List.map ~f:Lib.name
      in
      let project =
        List.fold_left libs ~init:None ~f:(fun acc lib ->
            let scope_for_key =
              let info = Lib.info lib in
              let status = Lib_info.status info in
              match status with
              | Private (scope_name, _) -> Some scope_name
              | Installed_private | Public _ | Installed -> None
            in
            Option.merge acc scope_for_key ~f:(fun a b ->
                assert (Dune_project.equal a b);
                a))
      in
      { pps; project_root = Option.map project ~f:Dune_project.root }
  end

  (* This mutable table is safe. Even though it can have stale entries remaining
     from previous runs, the entries themselves are correct, so this seems
     harmless apart from the lack of error in [decode] in this situation. *)
  let reverse_table : (Digest.t, Decoded.t) Table.t =
    Table.create (module Digest) 128

  let encode ({ Decoded.pps; project_root } as x) =
    let y = Digest.generic (pps, project_root) in
    match Table.find reverse_table y with
    | None ->
      Table.set reverse_table y x;
      y
    | Some x' ->
      if Decoded.equal x x' then y
      else
        User_error.raise
          [ Pp.textf "Hash collision between set of ppx drivers:"
          ; Pp.textf "- cache : %s" (Decoded.to_string x')
          ; Pp.textf "- fetch : %s" (Decoded.to_string x)
          ]

  let decode y =
    match Table.find reverse_table y with
    | Some x -> x
    | None ->
      User_error.raise
        [ Pp.textf "I don't know what ppx rewriters set %s correspond to."
            (Digest.to_string y)
        ]
end

module Driver = struct
  module M = struct
    module Info = struct
      let name = Sub_system_name.of_string "ppx.driver"

      type t =
        { loc : Loc.t
        ; flags : Ordered_set_lang.Unexpanded.t
        ; as_ppx_flags : Ordered_set_lang.Unexpanded.t
        ; lint_flags : Ordered_set_lang.Unexpanded.t
        ; main : string
        ; replaces : (Loc.t * Lib_name.t) list
        }

      type Sub_system_info.t += T of t

      let loc t = t.loc

      (* The syntax of the driver sub-system is part of the main dune syntax, so
         we simply don't create a new one.

         If we wanted to make the ppx system an extension, then we would create
         a new one. *)
      let syntax = Stanza.syntax

      open Dune_lang.Decoder

      let decode =
        fields
          (let+ loc = loc
           and+ flags = Ordered_set_lang.Unexpanded.field "flags"
           and+ as_ppx_flags =
             Ordered_set_lang.Unexpanded.field "as_ppx_flags"
               ~check:(Dune_lang.Syntax.since syntax (1, 2))
           and+ lint_flags = Ordered_set_lang.Unexpanded.field "lint_flags"
           and+ main = field "main" string
           and+ replaces =
             field "replaces" (repeat (located Lib_name.decode)) ~default:[]
           in
           { loc; flags; as_ppx_flags; lint_flags; main; replaces })

      let encode t =
        let open Dune_lang.Encoder in
        let lib (_loc, name) = Lib_name.encode name in
        ( (1, 0)
        , record_fields
          @@ [ field_i "flags" Ordered_set_lang.Unexpanded.encode t.flags
             ; field_i "lint_flags" Ordered_set_lang.Unexpanded.encode
                 t.lint_flags
             ; field "main" string t.main
             ; field_l "replaces" lib t.replaces
             ] )
    end

    type t =
      { info : Info.t
      ; lib : Lib.t
      ; replaces : t list Resolve.t
      }

    let desc ~plural = "ppx driver" ^ if plural then "s" else ""

    let desc_article = "a"

    let info t = t.info

    let lib t = t.lib

    let replaces t = t.replaces

    let instantiate ~resolve ~get lib (info : Info.t) =
      let open Memo.O in
      let+ replaces =
        Memo.parallel_map info.replaces ~f:(fun ((loc, name) as x) ->
            Resolve.Memo.bind (resolve x) ~f:(fun lib ->
                get ~loc lib >>| function
                | None ->
                  Resolve.fail
                    (User_error.make ~loc
                       [ Pp.textf "%S is not a %s" (Lib_name.to_string name)
                           (desc ~plural:false)
                       ])
                | Some t -> Resolve.return t))
        >>| Resolve.List.map ~f:Fun.id
      in
      { info; lib; replaces }

    let public_info t =
      Memo.return
        (let open Resolve.O in
        let+ replaces = t.replaces in
        { Info.loc = t.info.loc
        ; flags = t.info.flags
        ; as_ppx_flags = t.info.as_ppx_flags
        ; lint_flags = t.info.lint_flags
        ; main = t.info.main
        ; replaces =
            List.map2 t.info.replaces replaces ~f:(fun (loc, _) t ->
                (loc, Lib.name t.lib))
        })
  end

  include M
  include Sub_system.Register_backend (M)

  (* Where are we called from? *)
  type loc =
    | User_file of Loc.t * (Loc.t * Lib_name.t) list
    | Dot_ppx of Path.Build.t * Lib_name.t list

  let fail loc msg =
    match loc with
    | User_file (loc, _) -> Resolve.fail (User_error.make ~loc [ Pp.text msg ])
    | Dot_ppx (path, pps) ->
      Resolve.fail
        (User_error.make
           ~loc:(Loc.in_file (Path.build path))
           [ Pp.textf "Failed to create on-demand ppx rewriter for %s; %s"
               (String.enumerate_and (List.map pps ~f:Lib_name.to_string))
               (String.uncapitalize msg)
           ])

  let select libs ~loc =
    let open Memo.O in
    select_replaceable_backend libs ~replaces
    >>| Resolve.bind ~f:(function
          | Ok x -> Resolve.return x
          | Error Selection_error.No_backend_found ->
            let msg =
              match
                List.filter_map libs ~f:(fun lib ->
                    match Lib_name.to_string (Lib.name lib) with
                    | ("ocaml-migrate-parsetree" | "ppxlib" | "ppx_driver") as s
                      -> Some s
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
                   compatible with Dune. Examples of ppx rewriters that are \
                   compatible with Dune are ones using \
                   ocaml-migrate-parsetree, ppxlib or ppx_driver."
                  (String.enumerate_and (List.map pps ~f:Lib_name.to_string))
                  (match pps with
                  | [ _ ] -> "is"
                  | _ -> "are")
              | names ->
                sprintf
                  "No ppx driver were found.\n\
                   Hint: Try upgrading or reinstalling %s."
                  (String.enumerate_and names)
            in
            fail loc msg
          | Error (Too_many_backends ts) ->
            fail loc
              (sprintf "Too many incompatible ppx drivers were found: %s."
                 (String.enumerate_and
                    (List.map ts ~f:(fun t ->
                         Lib_name.to_string (Lib.name (lib t)))))))
end

let ppx_exe (ctx : Context.t) ~key =
  let build_dir = ctx.build_dir in
  Path.Build.relative build_dir (".ppx/" ^ key ^ "/ppx.exe")

let ppx_driver_exe (ctx : Context.t) libs =
  let key = Digest.to_string (Key.Decoded.of_libs libs |> Key.encode) in
  (* Make sure to compile ppx.exe for the compiling host. See: #2252, #2286 and
     #3698 *)
  ppx_exe ~key (Context.host ctx)

let get_cookies ~loc ~expander ~lib_name libs =
  let expander, library_name_cookie =
    match lib_name with
    | None -> (expander, None)
    | Some lib_name ->
      let library_name = Lib_name.Local.to_string lib_name in
      let bindings =
        Pform.Map.singleton (Var Library_name) [ Value.String library_name ]
      in
      ( Expander.add_bindings expander ~bindings
      , Some ("library-name", (library_name, Lib_name.of_local (loc, lib_name)))
      )
  in
  let+ cookies =
    Memo.List.concat_map libs ~f:(fun t ->
        let info = Lib.info t in
        let kind = Lib_info.kind info in
        match kind with
        | Normal -> Memo.return []
        | Ppx_rewriter { cookies } | Ppx_deriver { cookies } ->
          Memo.List.map cookies
            ~f:(fun { Lib_kind.Ppx_args.Cookie.name; value } ->
              let+ value = Expander.No_deps.expand_str expander value in
              (name, (value, Lib.name t))))
  in
  (match library_name_cookie with
  | None -> cookies
  | Some cookie -> cookie :: cookies)
  |> String.Map.of_list_reducei
       ~f:(fun name ((val1, lib1) as res) (val2, lib2) ->
         if String.equal val1 val2 then res
         else
           let lib1 = Lib_name.to_string lib1 in
           let lib2 = Lib_name.to_string lib2 in
           User_error.raise ~loc
             [ Pp.textf
                 "%s and %s have inconsistent requests for cookie %S; %s \
                  requests %S and %s requests %S"
                 lib1 lib2 name lib1 val1 lib2 val2
             ])
  |> String.Map.to_list_map ~f:(fun name (value, _) ->
         [ "--cookie"; sprintf "%s=%S" name value ])
  |> List.concat

let ppx_driver_and_flags_internal ~context ~expander ~loc ~lib_name ~flags libs
    =
  let open Action_builder.O in
  let* flags =
    let scope = Expander.scope expander in
    let dune_version = Scope.project scope |> Dune_project.dune_version in
    if dune_version <= (3, 2) then
      Action_builder.List.map ~f:(Expander.expand_str expander) flags
    else
      (* Allow list expansion *)
      Action_builder.List.concat_map flags
        ~f:(Expander.expand ~mode:Many expander)
      |> Action_builder.map
           ~f:
             (List.map
                ~f:(Value.to_string ~dir:(Path.build @@ Expander.dir expander)))
  in
  let+ cookies =
    Action_builder.of_memo (get_cookies ~loc ~lib_name ~expander libs)
  in
  let ppx_driver_exe = ppx_driver_exe context libs in
  (ppx_driver_exe, flags @ cookies)

let get_ppx_driver sctx ~loc ~expander ~scope ~lib_name ~flags pps =
  let open Action_builder.O in
  let* libs = Resolve.Memo.read (Lib.DB.resolve_pps (Scope.libs scope) pps) in
  let context = Super_context.context sctx in
  ppx_driver_and_flags_internal ~context ~loc ~expander ~lib_name ~flags libs

let ppx_exe_driver_and_flags ~context ~expander ~scope ~loc ~lib_name ~flags pps
    =
  let open Action_builder.O in
  let* libs = Resolve.Memo.read (Lib.DB.resolve_pps (Scope.libs scope) pps) in
  let* exe, flags =
    ppx_driver_and_flags_internal ~context ~expander ~loc ~lib_name ~flags libs
  in
  let* libs = Resolve.Memo.read (Lib.closure libs ~linking:true) in
  let+ driver =
    Action_builder.of_memo (Driver.select libs ~loc:(User_file (loc, pps)))
    >>= Resolve.read
  in
  (exe, driver, flags)

let add_corrected_suffix_binding expander suffix =
  let bindings =
    Pform.Map.singleton (Var Corrected_suffix) [ Value.String suffix ]
  in
  Expander.add_bindings expander ~bindings

let driver_flags expander ~corrected_suffix ~driver_flags ~standard =
  let expander = add_corrected_suffix_binding expander corrected_suffix in
  Expander.expand_and_eval_set expander driver_flags ~standard
