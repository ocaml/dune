open! Import

include Sub_system_intf

type Lib.Sub_system.t +=
    Gen of (Library_compilation_context.t -> unit)

let register_multi_backends (module M : Multi_backends) =
  let module X = struct
    open M

    type Sub_system_info.t += Backend_info of Backend.Info.t
    type Sub_system_info.t += Info of Info.t

    let () =
      Sub_system_info.register ()
        ~name:Backend.name
        ?short:(Option.map Backend.Info.short ~f:(fun x -> Backend_info x))
        ~of_sexp:(fun sexp -> Backend_info (Backend.Info.of_sexp sexp));
      Sub_system_info.register ()
        ~name:name
        ?short:(Option.map Info.short ~f:(fun x -> Info x))
        ~of_sexp:(fun sexp -> Info (Info.of_sexp sexp))

    type Lib.Sub_system.t += Backend of Backend.t

    let () =
      Lib.Sub_system.register ()
        ~name:Backend.name
        ~dump:(function
          | Backend x -> Backend.to_sexp x
          | _ -> assert false)
        ~instantiate:(fun db info ->
          match info with
          | Backend_info info ->
            Backend (Backend.instantiate db info)
          | _ -> assert false)

    let get_backend lib =
      Option.map (Lib.get_sub_system lib Backend.name) ~f:(function
        | Backend x -> x
        | _ -> assert false)

    let gen info (c : Library_compilation_context.t) =
      let open Result.O in
      let backends =
        let more_backends = Info.backends info in
        Result.all
          (List.map more_backends ~f:(fun (loc, name) ->
             match Lib.DB.find (Scope.libs c.scope) name with
             | Error reason ->
               Error { With_required_by.
                       data = Lib.Error.Library_not_available
                                { name
                                ; reason
                                }
                     ; required_by = [Loc loc]
                     }
             | Ok lib ->
               match get_backend lib with
               | Some x -> Ok x
               | None ->
                 (* XXX fix *)
                 Loc.fail loc "this is not a backend"))
        >>= fun more_backends ->
        Lib.Compile.requires c.compile_info >>= fun deps ->
        Lib.Compile.pps      c.compile_info >>= fun pps  ->
        Lib.closure (deps @ pps) >>| fun deps ->
        List.filter_map deps ~f:get_backend @ more_backends
      in
      match backends with
      | Ok [] ->
        Super_context.prefix_rules c.super_context
          (Build.fail { fail = fun () ->
             Loc.fail c.stanza.buildable.loc
               "No backend found for (%s)"
               (Sub_system_name.to_string name) })
          ~f:(fun () -> gen_rules c ~info ~backends:[])
      | Ok backends -> gen_rules c ~info ~backends
      | Error e ->
        Super_context.prefix_rules c.super_context
          (Build.fail { fail = fun () -> raise (Lib.Error e) })
          ~f:(fun () -> gen_rules c ~info ~backends:[])

    let () =
      Lib.Sub_system.register ()
        ~name
        ~instantiate:(fun _db info ->
          match info with
          | Info info -> Gen (gen info)
          | _ -> assert false)
  end in
  ()

let gen_rules (c : Library_compilation_context.t) =
  List.iter (Lib.Compile.sub_systems c.compile_info) ~f:(function
    | Gen gen -> gen c
    | _ -> ())
