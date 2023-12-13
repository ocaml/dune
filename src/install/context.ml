open Import

let install_context =
  let name = Context_name.of_string "install" in
  Dune_engine.Build_context.create ~name
;;

let dir ~context =
  let context = Context_name.to_string context in
  Path.Build.relative (Context_name.build_dir install_context.name) context
;;

let lib_root ~context = Path.Build.relative (dir ~context) "lib"
let bin_dir ~context = Path.Build.relative (dir ~context) "bin"

let lib_dir ~context ~package =
  Path.Build.relative (lib_root ~context) (Package_name.to_string package)
;;

let of_path path =
  match Dune_engine.Dpath.analyse_dir (Path.build path) with
  | Build (Regular (With_context (name, src))) ->
    Some
      (if Context_name.equal name install_context.name
       then Context_name.of_string (Path.Source.basename src)
       else name)
  | Build (Anonymous_action (With_context (name, _))) -> Some name
  | _ -> None
;;

type analyze_path =
  | Invalid
  | Install of Context_name.t * Path.Source.t
  | Normal of Context_name.t * Path.Source.t

let analyze_path ctx_name source =
  match Context_name.equal ctx_name install_context.name with
  | false -> Normal (ctx_name, source)
  | true ->
    (match Path.Source.split_first_component source with
     | None -> Invalid
     | Some (ctx, path) -> Install (Context_name.of_string ctx, Path.Source.of_local path))
;;
