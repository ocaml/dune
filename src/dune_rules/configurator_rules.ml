open Import
open Memo.O

let dot_dune_dir (t : Build_context.t) = Path.Build.relative t.build_dir ".dune"
let configurator_v1 t = Path.Build.relative (dot_dune_dir t) "configurator"
let configurator_v2 t = Path.Build.relative (dot_dune_dir t) "configurator.v2"

(* We store this so that library such as dune-configurator can read things
   runtime. Ideally, this should be created on-demand if we run a program linked
   against configurator, however we currently don't support this kind of
   "runtime dependencies" so we just do it eagerly. *)
let gen_rules (ctx : Build_context.t) (ocaml : Ocaml_toolchain.t Action_builder.t) =
  let ocaml_and_ocaml_config_vars =
    Action_builder.map ocaml ~f:(fun (ocaml : Ocaml_toolchain.t) ->
      ( Path.to_absolute_filename ocaml.ocamlc
      , Ocaml_config.Vars.to_list ocaml.ocaml_config_vars ))
  in
  let* () =
    let fn = configurator_v1 ctx in
    (let open Action_builder.O in
     let+ ocamlc, ocaml_config_vars = ocaml_and_ocaml_config_vars in
     (let open Dune_lang.Encoder in
      record_fields
        [ field "ocamlc" string ocamlc
        ; field_l "ocaml_config_vars" (pair string string) ocaml_config_vars
        ])
     |> List.map ~f:(fun x -> Dune_lang.to_string x ^ "\n")
     |> String.concat ~sep:""
     |> Action.write_file fn
     |> Action.Full.make)
    |> Rule.make ~targets:(Targets.File.create fn)
    |> Rules.Produce.rule
  in
  let fn = configurator_v2 ctx in
  (let open Action_builder.O in
   let+ ocamlc, ocaml_config_vars = ocaml_and_ocaml_config_vars in
   (let open Sexp in
    let ocaml_config_vars =
      Sexp.List (List.map ocaml_config_vars ~f:(fun (k, v) -> List [ Atom k; Atom v ]))
    in
    List
      [ List [ Atom "ocamlc"; Atom ocamlc ]
      ; List [ Atom "ocaml_config_vars"; ocaml_config_vars ]
      ])
   |> Csexp.to_string
   |> Action.write_file fn
   |> Action.Full.make)
  |> Rule.make ~targets:(Targets.File.create fn)
  |> Rules.Produce.rule
;;

let force_files =
  Memo.lazy_ ~name:"force-configuration-files" (fun () ->
    let* ctxs = Context.DB.all () in
    let files =
      List.concat_map ctxs ~f:(fun t ->
        let build_context = Context.build_context t in
        [ Path.build (configurator_v1 build_context)
        ; Path.build (configurator_v2 build_context)
        ])
    in
    Memo.parallel_iter files ~f:Build_system.build_file)
;;

let gen_rules (name : Context_name.t) =
  let ocaml =
    Action_builder.of_memo
    @@ let* context = Context.DB.get name in
       Context.ocaml context
  in
  let build_context = Build_context.create ~name in
  gen_rules build_context ocaml
;;
