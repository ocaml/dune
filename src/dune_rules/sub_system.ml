open! Dune_engine
open! Stdune
open! Import
include Sub_system_intf
open Memo.Build.O

module Register_backend (M : Backend) = struct
  include Sub_system_info.Register (M.Info)

  include Lib.Sub_system.Register (struct
    include M

    type Lib.Sub_system.t += T of t

    let public_info = Some public_info
  end)

  let top_closure l ~deps =
    match
      Lib.L.top_closure l ~key:M.lib ~deps:(fun t ->
          match deps t with
          | Ok l -> l
          | Error e -> raise_notrace e)
    with
    | Ok _ as res -> res
    | Error _ ->
      (* Lib.t values can't be cyclic, so we can't have cycles here *)
      assert false
    | exception exn -> Error exn

  include Comparable.Make (struct
    type t = M.t

    let to_dyn _ = Dyn.opaque

    let compare a b =
      Lib.Id.compare (Lib.unique_id (M.lib a)) (Lib.unique_id (M.lib b))
  end)

  let resolve db (loc, name) =
    let open Memo.Build.O in
    match Lib.DB.resolve db (loc, name) with
    | Error _ as err -> Memo.Build.return err
    | Ok lib -> (
      get lib >>| function
      | None ->
        Error
          (User_error.E
             ( User_error.make ~loc
                 [ Pp.textf "%s is not %s %s" (Lib_name.to_string name)
                     M.desc_article (M.desc ~plural:false)
                 ]
             , None ))
      | Some t -> Ok t)

  module Selection_error = struct
    type t =
      | Too_many_backends of M.t list
      | No_backend_found
      | Other of exn

    let to_exn t ~loc =
      match t with
      | Too_many_backends backends ->
        User_error.E
          ( User_error.make ~loc
              [ Pp.textf "Too many independent %s found:" (M.desc ~plural:true)
              ; Pp.enumerate backends ~f:(fun t ->
                    let lib = M.lib t in
                    let info = Lib.info lib in
                    let src_dir = Lib_info.src_dir info in
                    Pp.textf "%S in %s"
                      (Lib_name.to_string (Lib.name lib))
                      (Path.to_string_maybe_quoted src_dir))
              ]
          , None )
      | No_backend_found ->
        User_error.E
          ( User_error.make ~loc
              [ Pp.textf "No %s found." (M.desc ~plural:false) ]
          , None )
      | Other exn -> exn

    let or_exn res ~loc =
      match res with
      | Ok _ as x -> x
      | Error t -> Error (to_exn t ~loc)

    let wrap = function
      | Ok _ as x -> x
      | Error exn -> Error (Other exn)
  end

  open Selection_error

  let written_by_user_or_scan ~written_by_user ~to_scan =
    let open Memo.Build.O in
    (match written_by_user with
    | Some l -> Memo.Build.return l
    | None ->
      Memo.Build.parallel_map to_scan ~f:get >>| List.filter_map ~f:Fun.id)
    >>| function
    | [] -> Error No_backend_found
    | l -> Ok l

  let select_extensible_backends ?written_by_user ~extends to_scan =
    Memo.Build.map (written_by_user_or_scan ~written_by_user ~to_scan)
      ~f:(fun backends ->
        let open Result.O in
        let* backends = backends in
        let* backends = wrap (top_closure backends ~deps:extends) in
        let roots =
          let all = Set.of_list backends in
          List.fold_left backends ~init:all ~f:(fun acc t ->
              Set.diff acc (Set.of_list (Result.ok_exn (extends t))))
          |> Set.to_list
        in
        if List.length roots = 1 then
          Ok backends
        else
          Error (Too_many_backends roots))

  let select_replaceable_backend ?written_by_user ~replaces to_scan =
    Memo.Build.map (written_by_user_or_scan ~written_by_user ~to_scan)
      ~f:(fun backends ->
        let open Result.O in
        let* backends = backends in
        let* replaced_backends =
          wrap (Result.List.concat_map backends ~f:replaces)
        in
        match
          Set.diff (Set.of_list backends) (Set.of_list replaced_backends)
          |> Set.to_list
        with
        | [ b ] -> Ok b
        | l -> Error (Too_many_backends l))
end

type Lib.Sub_system.t +=
  | Gen of (Library_compilation_context.t -> unit Memo.Build.t)

module Register_end_point (M : End_point) = struct
  include Sub_system_info.Register (M.Info)

  let gen info (c : Library_compilation_context.t) =
    let open Memo.Build.O in
    let* backends =
      let ( let& ) t f =
        Memo.Build.bind t ~f:(function
          | Error _ as err -> Memo.Build.return err
          | Ok x -> f x)
      in
      let& deps =
        Memo.Build.return (Lib.Compile.direct_requires c.compile_info)
      in
      let& pps = Memo.Build.return (Lib.Compile.pps c.compile_info) in
      let& written_by_user =
        match M.Info.backends info with
        | None -> Memo.Build.return (Ok None)
        | Some l ->
          let+ l =
            Memo.Build.parallel_map l
              ~f:(M.Backend.resolve (Scope.libs c.scope))
          in
          Result.map (Result.List.map l ~f:Fun.id) ~f:Option.some
      in
      let+ backends =
        M.Backend.select_extensible_backends ?written_by_user
          ~extends:M.Backend.extends (deps @ pps)
      in
      M.Backend.Selection_error.or_exn ~loc:(M.Info.loc info) backends
    in
    let fail, backends =
      match backends with
      | Ok backends -> (None, backends)
      | Error e -> (Some { fail = (fun () -> raise e) }, [])
    in
    match fail with
    | None -> M.gen_rules c ~info ~backends
    | Some fail ->
      Build_system.prefix_rules (Action_builder.fail fail) ~f:(fun () ->
          M.gen_rules c ~info ~backends)

  include Lib.Sub_system.Register (struct
    module Info = M.Info

    type t = Library_compilation_context.t -> unit Memo.Build.t

    type Lib.Sub_system.t += T = Gen

    let instantiate ~resolve:_ ~get:_ _id info = Memo.Build.return (gen info)

    let public_info = None
  end)
end

let gen_rules (c : Library_compilation_context.t) =
  let* l = Lib.Compile.sub_systems c.compile_info in
  Memo.Build.parallel_iter l ~f:(function
    | Gen gen -> gen c
    | _ -> Memo.Build.return ())
