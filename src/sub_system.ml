open! Import

include Sub_system_intf

module Register_backend(M : Backend) = struct
  include Jbuild.Sub_system_info.Register(M.Info)
  include Lib.Sub_system.Register(struct
      include M
      type Lib.Sub_system.t += T of t
      let to_sexp = Some to_sexp
    end)

  let top_closure l ~deps =
    match
      Top_closure.Int.top_closure l
        ~key:(fun t -> Lib.unique_id (M.lib t))
        ~deps:(fun t ->
          match deps t with
          | Ok l    -> l
          | Error e -> raise_notrace e)
    with
    | Ok _ as res -> res
    | Error _ ->
      (* Lib.t values can't be cyclic, so we can't have cycles here *)
      assert false
    | exception exn -> Error exn

  module Set =
    Set.Make(struct
      type t = M.t
      let compare a b =
        compare
          (Lib.unique_id (M.lib a))
          (Lib.unique_id (M.lib b))
    end)

  let resolve db (loc, name) =
    let open Result.O in
    Lib.DB.resolve db (loc, name) >>= fun lib ->
    match get lib with
    | None ->
      Error (Loc.exnf loc "%S is not %s %s" name M.desc_article
               (M.desc ~plural:false))
    | Some t -> Ok t

  let written_by_user_or_scan ~loc ~written_by_user ~to_scan =
    match
      match written_by_user with
      | Some l -> l
      | None   -> List.filter_map to_scan ~f:get
    with
    | [] ->
      Error
        (Loc.exnf loc "No %s found." (M.desc ~plural:false))
    | l -> Ok l

  let too_many_backends ~loc backends =
    Loc.exnf loc
      "Too many independant %s found:\n%s"
      (M.desc ~plural:true)
      (String.concat ~sep:"\n"
         (List.map backends ~f:(fun t ->
            let lib = M.lib t in
            sprintf "- %S in %s"
              (Lib.name lib)
              (Path.to_string_maybe_quoted (Lib.src_dir lib)))))

  let select_extensible_backends ~loc ?written_by_user ~extends to_scan =
    let open Result.O in
    written_by_user_or_scan ~loc ~written_by_user ~to_scan
    >>= fun backends ->
    top_closure backends ~deps:extends
    >>= fun backends ->
    let roots =
      let all = Set.of_list backends in
      List.fold_left backends ~init:all ~f:(fun acc t ->
        Set.diff acc (Set.of_list (Result.ok_exn (extends t))))
      |> Set.to_list
    in
    if List.length roots = 1 then
      Ok backends
    else
      Error (too_many_backends ~loc roots)

  let select_replaceable_backend ~loc ?written_by_user ~replaces to_scan =
    let open Result.O in
    written_by_user_or_scan ~loc ~written_by_user ~to_scan
    >>= fun backends ->
    Result.concat_map backends ~f:replaces
    >>= fun replaced_backends ->
    match
      Set.diff (Set.of_list backends) (Set.of_list replaced_backends)
      |> Set.to_list
    with
    | [b] -> Ok b
    | l   -> Error (too_many_backends ~loc l)
end

type Lib.Sub_system.t +=
    Gen of (Library_compilation_context.t -> unit)

module Register_end_point(M : End_point) = struct
  include Jbuild.Sub_system_info.Register(M.Info)

  let gen info (c : Library_compilation_context.t) =
    let open Result.O in
    let backends =
      Lib.Compile.direct_requires c.compile_info >>= fun deps ->
      Lib.Compile.pps             c.compile_info >>= fun pps  ->
      (match M.Info.backends info with
       | None -> Ok None
       | Some l ->
         Result.all (List.map l ~f:(M.Backend.resolve (Scope.libs c.scope)))
         >>| Option.some)
      >>= fun written_by_user ->
      M.Backend.select_extensible_backends
        ~loc:(M.Info.loc info)
        ?written_by_user
        ~extends:M.Backend.extends
        (deps @ pps)
    in
    let fail, backends =
      match backends with
      | Ok backends -> (None, backends)
      | Error e ->
        (Some { fail = fun () -> raise e },
         [])
    in
    match fail with
    | None -> M.gen_rules c ~info ~backends
    | Some fail ->
      Super_context.prefix_rules c.super_context (Build.fail fail)
        ~f:(fun () -> M.gen_rules c ~info ~backends)

  include
    Lib.Sub_system.Register
      (struct
        module Info = M.Info
        type t = Library_compilation_context.t -> unit
        type Lib.Sub_system.t += T = Gen
        let instantiate ~resolve:_ ~get:_ _id info = gen info
        let to_sexp = None
      end)
end

let gen_rules (c : Library_compilation_context.t) =
  List.iter (Lib.Compile.sub_systems c.compile_info) ~f:(function
    | Gen gen -> gen c
    | _ -> ())
