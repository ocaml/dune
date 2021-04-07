open! Dune_engine
open! Stdune
open Import

let () = Inline_tests.linkme

type build_system =
  { conf : Dune_load.conf
  ; contexts : Context.t list
  ; scontexts : Super_context.t Context_name.Map.t
  }

let init_build_system ~stats ~sandboxing_preference ~caching ~conf ~contexts =
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
      ~contexts:(List.map ~f:Context.build_context contexts)
      ?caching ()
  in
  List.iter contexts ~f:Context.init_configurator;
  let+ scontexts = Gen_rules.init () in
  { conf; contexts; scontexts }

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
