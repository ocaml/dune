open! Import

include Sub_system_intf

type Lib.Sub_system.t +=
    Gen of (Library_compilation_context.t -> unit)

let register_multi_backends (module M : Multi_backends) =
  let module X = struct
    include Jbuild.Sub_system_info.Register(M.Backend.Info)
    include Jbuild.Sub_system_info.Register(M.Info)

    module Backend = struct
      module M = struct
        include M.Backend
        type Lib.Sub_system.t += T of t
        let to_sexp = Some to_sexp
      end
      include M
      include Lib.Sub_system.Register(M)
    end

    let gen info (c : Library_compilation_context.t) =
      let open Result.O in
      let backends =
        let more_backends = M.Info.backends info in
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
               match Backend.get lib with
               | Some x -> Ok x
               | None ->
                 (* XXX this should raise immediately, but the error
                    is of the wrong type. *)
                 Loc.fail loc
                   "Library %S is not a backend for the %S sub-system"
                   name
                   (Sub_system_name.to_string M.Info.name)))
        >>= fun more_backends ->
        Lib.Compile.requires c.compile_info >>= fun deps ->
        Lib.Compile.pps      c.compile_info >>= fun pps  ->
        (* We need to take the transitive closure to ensure the order
           is correct *)
        Lib.closure (deps @ pps) >>| fun deps ->
        List.filter_map deps ~f:Backend.get @ more_backends
      in
      let fail, backends =
        match backends with
        | Ok [] ->
          (Some { fail = fun () ->
             Loc.fail (M.Info.loc info)
               "No backend found for (%s)"
               (Sub_system_name.to_string M.Info.name) },
           [])
        | Ok backends -> (None, backends)
        | Error e ->
          (Some { fail = fun () -> raise (Lib.Error e) },
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
          let instantiate _db info = gen info
          let to_sexp = None
        end)
  end in
  ()

let gen_rules (c : Library_compilation_context.t) =
  List.iter (Lib.Compile.sub_systems c.compile_info) ~f:(function
    | Gen gen -> gen c
    | _ -> ())
