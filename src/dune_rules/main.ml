open! Dune_engine
open! Stdune
open Import

let () = Inline_tests.linkme

type workspace =
  { contexts : Context.t list
  ; conf : Dune_load.conf
  }

type build_system =
  { workspace : workspace
  ; scontexts : Super_context.t Context_name.Map.t
  }

let package_install_file w pkg =
  match Package.Name.Map.find w.conf.packages pkg with
  | None -> Error ()
  | Some p ->
    let name = Package.name p in
    let dir = Package.dir p in
    Ok
      (Path.Source.relative dir
         (Utils.install_file ~package:name ~findlib_toolchain:None))

let scan_workspace () =
  let open Memo.Build.O in
  let* conf = Dune_load.load () in
  let+ contexts = Context.DB.all () in
  List.iter contexts ~f:(fun (ctx : Context.t) ->
      let open Pp.O in
      Log.info
        [ Pp.box ~indent:1
            (Pp.text "Dune context:" ++ Pp.cut ++ Dyn.pp (Context.to_dyn ctx))
        ]);
  { contexts; conf }

let init_build_system ?stats ?only_packages ~sandboxing_preference ?caching
    ?build_mutex w =
  let open Fiber.O in
  Build_system.reset ();
  let promote_source ?chmod ~src ~dst ctx =
    let conf = Artifact_substitution.conf_of_context ctx in
    let src = Path.build src in
    let dst = Path.source dst in
    Artifact_substitution.copy_file ?chmod ~src ~dst ~conf ()
  in
  let* () =
    Build_system.init ~stats ~sandboxing_preference ~promote_source
      ~contexts:(List.map ~f:Context.build_context w.contexts)
      ?caching ?build_mutex ()
  in
  List.iter w.contexts ~f:Context.init_configurator;
  let+ scontexts = Gen_rules.init w.conf ~contexts:w.contexts ?only_packages in
  { workspace = w; scontexts }

let find_context_exn t ~name =
  match List.find t.contexts ~f:(fun c -> Context_name.equal c.name name) with
  | Some ctx -> ctx
  | None ->
    User_error.raise
      [ Pp.textf "Context %S not found!" (Context_name.to_string name) ]

let find_scontext_exn t ~name =
  match Context_name.Map.find t.scontexts name with
  | Some ctx -> ctx
  | None ->
    User_error.raise
      [ Pp.textf "Context %S not found!" (Context_name.to_string name) ]
