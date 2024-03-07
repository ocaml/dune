open Import
open Memo.O
module P = Ocaml.Variant
module Ps = Ocaml.Variant.Set

module File = struct
  type t =
    { vars : Vars.t
    ; preds : Ps.t
    }

  let to_dyn { vars; preds } =
    let open Dyn in
    record [ "vars", Vars.to_dyn vars; "preds", Ps.to_dyn preds ]
  ;;

  let load config_file =
    let load p =
      let+ meta = Meta.load ~name:None p in
      Vars.of_meta_rules meta.vars
    in
    let+ vars =
      let* vars =
        Fs_memo.file_exists config_file
        >>= function
        | true -> load config_file
        | false -> Memo.return Vars.empty
      in
      let config_dir = Path.Outside_build_dir.extend_basename config_file ~suffix:".d" in
      Fs_memo.is_directory config_dir
      >>= function
      | Ok false | Error (_ : Unix.error * _ * _) -> Memo.return vars
      | Ok true ->
        Fs_memo.dir_contents config_dir
        >>= (function
         | Ok dir_contents ->
           let+ all_vars =
             Memo.parallel_map
               (Fs_cache.Dir_contents.to_list dir_contents)
               ~f:(fun (p, _kind) ->
                 let p = Path.Outside_build_dir.relative config_dir p in
                 load p)
           in
           List.fold_left all_vars ~init:vars ~f:(fun acc vars ->
             Vars.union acc vars ~f:(fun _ x y -> Some (Rules.union x y)))
         | Error (_ : Unix.error * _ * _) -> Memo.return vars)
    in
    { vars; preds = Ps.empty }
  ;;

  let get { vars; preds } var = Vars.get vars var preds
  let toolchain t ~toolchain = { t with preds = Ps.singleton (P.make toolchain) }
end

type t =
  { config : File.t
  ; ocamlpath : Path.t list Memo.t
  ; which : string -> Path.t option Memo.t
  ; toolchain : string option
  }

let to_dyn { config; ocamlpath = _; toolchain; which = _ } =
  let open Dyn in
  record [ "config", File.to_dyn config; "toolchain", option string toolchain ]
;;

let ocamlpath_sep =
  if Sys.cygwin then (* because that's what ocamlfind expects *)
                  ';' else Bin.path_sep
;;

let ocamlpath_var = "OCAMLPATH"
let ocamlfind_ignore_dups_in = "OCAMLFIND_IGNORE_DUPS_IN"
let path_var = Bin.parse_path ~sep:ocamlpath_sep
let ocamlpath_of_env env = Env.get env ocamlpath_var |> Option.map ~f:path_var

let set_toolchain t ~toolchain =
  match t.toolchain with
  | None ->
    { t with config = File.toolchain t.config ~toolchain; toolchain = Some toolchain }
  | Some old_toolchain ->
    Code_error.raise
      "Findlib_config.set_toolchain: cannot set toolchain twice"
      [ "old_toolchain", Dyn.string old_toolchain; "toolchain", Dyn.string toolchain ]
;;

let ocamlpath t =
  let+ ocamlpath = t.ocamlpath in
  match File.get t.config "path" with
  | None -> ocamlpath
  | Some p -> ocamlpath @ path_var p
;;

let tool t ~prog =
  match File.get t.config prog with
  | None -> Memo.return None
  | Some s ->
    (match Filename.analyze_program_name s with
     | In_path -> t.which s
     | Relative_to_current_dir ->
       User_error.raise
         [ Pp.textf
             "The effective Findlib configuration specifies the relative path %S for the \
              program %S. This is currently not supported."
             s
             prog
         ]
     | Absolute -> Memo.return (Some (Path.of_filename_relative_to_initial_cwd s)))
;;

let ocamlfind_config_path ~env ~which ~findlib_toolchain =
  let open Memo.O in
  let+ path =
    (* How dune finds the [findlib.conf] file:

       - if [$OCAMLFIND_CONF] is set, prefer its value
       - if the [ocamlfind] binary is in [$PATH], run
         [ocamlfind printconf conf] to get its value

       dune attempts to find [ocaml], [ocamlopt], etc. in the findlib
       configuration for the toolchain first to account for cross-compilation
       use cases. It then falls back to searching in [$PATH] *)
    match Env.get env "OCAMLFIND_CONF" with
    | Some s -> Memo.return (Some s)
    | None ->
      (match findlib_toolchain with
       | None ->
         (* If this variable isn't set, we don't bother with the binary. We'd
            like dune not to use the ocamlfind binary outside of cross
            compilation *)
         Memo.return None
       | Some _ ->
         which "ocamlfind"
         >>= (function
          | None -> Memo.return None
          | Some fn ->
            Process.run_capture_line ~display:Quiet ~env Strict fn [ "printconf"; "conf" ]
            |> Memo.of_reproducible_fiber
            |> Memo.map ~f:Option.some))
  in
  (* From http://projects.camlcity.org/projects/dl/findlib-1.9.6/doc/ref-html/r865.html
     This variable overrides the location of the configuration file
     findlib.conf. It must contain the absolute path name of this file. *)
  Option.map path ~f:Path.External.of_string
;;

let discover_from_env ~env ~which ~ocamlpath ~findlib_toolchain =
  let open Memo.O in
  ocamlfind_config_path ~env ~which ~findlib_toolchain
  >>= function
  | None -> Memo.return None
  | Some config ->
    let+ config = File.load (External config) in
    let base = { config; ocamlpath; which; toolchain = None } in
    Some
      (match findlib_toolchain with
       | None -> base
       | Some toolchain -> set_toolchain base ~toolchain)
;;

let env t =
  let preds = Ps.add t.config.preds (P.make "env") in
  Vars.to_string_map ~f:(Rules.interpret ~preds) t.config.vars |> Env.of_string_map
;;
