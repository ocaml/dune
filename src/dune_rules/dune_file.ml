open Import

type t =
  { dir : Path.Source.t
  ; project : Dune_project.t
  ; stanzas : Stanza.t list
  }

let dir t = t.dir
let stanzas t = t.stanzas
let project t = t.project

module Mask = struct
  type 'a t =
    | True
    | Fun of ('a -> bool)

  let combine x y =
    match x, y with
    | True, x -> x
    | x, True -> x
    | Fun f, Fun g -> Fun (fun x -> f x && g x)
  ;;

  let filter_stanzas t list =
    match t with
    | True -> list
    | Fun f ->
      List.filter_map list ~f:(fun stanza ->
        if f stanza
        then Some stanza
        else (
          match Stanza.repr stanza with
          | Library.T l ->
            Library_redirect.Local.of_private_lib l
            |> Option.map ~f:Library_redirect.Local.make_stanza
          | _ -> None))
  ;;

  let of_only_packages_mask mask =
    match mask with
    | None -> True
    | Some visible_pkgs ->
      Fun
        (fun stanza ->
          match Stanzas.stanza_package stanza with
          | None -> true
          | Some package ->
            let name = Package.name package in
            Package.Name.Map.mem visible_pkgs name)
  ;;

  let is_promoted_rule =
    let is_promoted_mode version = function
      | Rule.Mode.Promote { only = None; lifetime; _ } ->
        if version >= (3, 5)
        then (
          match lifetime with
          | Unlimited -> true
          | Until_clean -> false)
        else true
      | _ -> false
    in
    fun version rule ->
      match Stanza.repr rule with
      | Rule_conf.T { mode; _ } | Menhir_stanza.T { mode; _ } ->
        is_promoted_mode version mode
      | _ -> false
  ;;

  let ignore_promote project =
    match !Clflags.ignore_promoted_rules with
    | false -> True
    | true ->
      let version = Dune_project.dune_version project in
      Fun (fun stanza -> not (is_promoted_rule version stanza))
  ;;
end

(* XXX this is needed for evaluating includes generated by dune files written
   in OCaml syntax.*)
let rec parse_file_includes ~stanza_parser ~context sexps =
  List.concat_map sexps ~f:(Dune_lang.Decoder.parse stanza_parser Univ_map.empty)
  |> Memo.List.concat_map ~f:(fun stanza ->
    match Stanza.repr stanza with
    | Stanzas.Include.T (loc, fn) ->
      let open Memo.O in
      let* sexps, context = Include_stanza.load_sexps ~context (loc, fn) in
      parse_file_includes ~stanza_parser ~context sexps
    | _ -> Memo.return [ stanza ])
;;

let parse ~file ~dir (project : Dune_project.t) sexps =
  let stanza_parser = Dune_project.stanza_parser project in
  let warnings = Warning_emit.Bag.create () in
  let stanza_parser = Warning_emit.Bag.set warnings stanza_parser in
  let open Memo.O in
  let* stanzas =
    let context =
      Include_stanza.in_file
      @@
      match file with
      | Some f -> f
      | None ->
        (* TODO this is wrong *)
        Path.Source.relative dir Dune_file0.fname
    in
    parse_file_includes ~stanza_parser ~context sexps
  in
  let (_ : bool) =
    List.fold_left stanzas ~init:false ~f:(fun env stanza ->
      match Stanza.repr stanza with
      | Dune_env.T e ->
        if env
        then
          User_error.raise
            ~loc:e.loc
            [ Pp.text "The 'env' stanza cannot appear more than once" ]
        else true
      | _ -> env)
  in
  let+ () = Warning_emit.Bag.emit_all warnings in
  stanzas
;;

let parse sexps ~mask ~dir ~file ~project =
  let open Memo.O in
  let+ stanzas = parse ~file ~dir project sexps in
  let stanzas = Mask.filter_stanzas mask stanzas in
  { dir; project; stanzas }
;;

module Make_fold (M : Monad.S) = struct
  open M.O

  let rec fold_stanzas l ~init ~f =
    match l with
    | [] -> M.return init
    | t :: l -> inner_fold t t.stanzas l ~init ~f

  and inner_fold t inner_list l ~init ~f =
    match inner_list with
    | [] -> fold_stanzas l ~init ~f
    | x :: inner_list ->
      let* init = f t x init in
      inner_fold t inner_list l ~init ~f
  ;;
end

module Memo_fold = Make_fold (Memo)
module Id_fold = Make_fold (Monad.Id)

let fold_stanzas t ~init ~f = Id_fold.fold_stanzas t ~init ~f
let to_dyn = Dyn.opaque

let find_stanzas t key =
  (* CR-rgrinberg: save a map to represent the stanzas to make this fast. *)
  List.filter_map t.stanzas ~f:(Stanza.Key.get key)
;;

module Jbuild_plugin : sig
  val create_plugin_wrapper
    :  Context_name.t
    -> Ocaml_config.t
    -> exec_dir:Path.t
    -> plugin:Path.Outside_build_dir.t
    -> wrapper:Path.Build.t
    -> target:Path.Build.t
    -> unit Memo.t
end = struct
  let replace_in_template =
    let template =
      lazy
        (let marker name =
           let open Re in
           [ str "(*$"; rep space; str name; rep space; str "$*)" ] |> seq |> Re.mark
         in
         let mark_start, marker_start = marker "begin_vars" in
         let mark_end, marker_end = marker "end_vars" in
         let markers = Re.alt [ marker_start; marker_end ] in
         let invalid_template stage =
           Code_error.raise
             "Jbuild_plugin.replace_in_template: invalid template"
             [ "stage", Dyn.string stage ]
         in
         let rec parse1 = function
           | `Text s :: xs -> parse2 s xs
           | xs -> parse2 "" xs
         and parse2 prefix = function
           | `Delim ds :: `Text _ :: `Delim de :: xs
             when Re.Mark.test ds mark_start && Re.Mark.test de mark_end ->
             parse3 prefix xs
           | _ -> invalid_template "parse2"
         and parse3 prefix = function
           | [] -> prefix, ""
           | [ `Text suffix ] -> prefix, suffix
           | _ -> invalid_template "parse3"
         in
         let tokens = Re.split_full (Re.compile markers) Assets.jbuild_plugin_ml in
         parse1 tokens)
    in
    fun t ->
      let prefix, suffix = Lazy.force template in
      sprintf "%s%s%s" prefix t suffix
  ;;

  let write
    oc
    ~(context : Context_name.t)
    ~ocaml_config
    ~target
    ~exec_dir
    ~plugin
    ~plugin_contents
    =
    let ocamlc_config =
      let vars =
        Ocaml_config.to_list ocaml_config
        |> List.map ~f:(fun (k, v) -> k, Ocaml_config.Value.to_string v)
      in
      let longest = String.longest_map vars ~f:fst in
      List.map vars ~f:(fun (k, v) -> sprintf "%-*S , %S" (longest + 2) k v)
      |> String.concat ~sep:"\n      ; "
    in
    let vars =
      Printf.sprintf
        {|let context = %S
        let ocaml_version = %S
        let send_target = %S
        let ocamlc_config = [ %s ]
        |}
        (Context_name.to_string context)
        (Ocaml_config.version_string ocaml_config)
        (Path.reach ~from:exec_dir (Path.build target))
        ocamlc_config
    in
    Printf.fprintf
      oc
      "module Jbuild_plugin : sig\n%s\nend = struct\n%s\nend\n# 1 %S\n%s"
      Assets.jbuild_plugin_mli
      (replace_in_template vars)
      (Path.Outside_build_dir.to_string plugin)
      plugin_contents
  ;;

  let check_no_requires path str =
    List.iteri (String.split str ~on:'\n') ~f:(fun n line ->
      match Scanf.sscanf line "#require %S" Fun.id with
      | Error () -> ()
      | Ok (_ : string) ->
        let loc : Loc.t =
          let start : Lexing.position =
            { pos_fname = Path.to_string path; pos_lnum = n; pos_cnum = 0; pos_bol = 0 }
          in
          Loc.create ~start ~stop:{ start with pos_cnum = String.length line }
        in
        User_error.raise
          ~loc
          [ Pp.text "#require is no longer supported in dune files."
          ; Pp.text
              "You can use the following function instead of Unix.open_process_in:\n\n\
              \  (** Execute a command and read it's output *)\n\
              \  val run_and_read_lines : string -> string list"
          ])
  ;;

  let create_plugin_wrapper context ocaml_config ~exec_dir ~plugin ~wrapper ~target =
    let open Memo.O in
    let+ plugin_contents = Fs_memo.file_contents plugin in
    Io.with_file_out (Path.build wrapper) ~f:(fun oc ->
      write oc ~context ~ocaml_config ~target ~exec_dir ~plugin ~plugin_contents);
    check_no_requires (Path.outside_build_dir plugin) plugin_contents
  ;;
end

module Script = struct
  open Memo.O

  type t =
    { dir : Path.Source.t
    ; file : Path.Source.t
    ; project : Dune_project.t
    ; from_parent : Dune_lang.Ast.t list
    }

  (* CR-rgrinberg: context handling code should be aware of this special
     directory *)
  let generated_dune_files_dir = Path.Build.relative Path.Build.root ".dune"

  let eval_one ~mask ~context { dir; file; project; from_parent } =
    let generated_dune_file =
      Path.Build.append_source
        (Path.Build.relative generated_dune_files_dir (Context_name.to_string context))
        file
    in
    let wrapper = Path.Build.extend_basename generated_dune_file ~suffix:".ml" in
    generated_dune_file |> Path.build |> Path.parent |> Option.iter ~f:Path.mkdir_p;
    let* context = Context.DB.get context in
    let* ocaml = Context.ocaml context in
    let* () =
      Jbuild_plugin.create_plugin_wrapper
        (Context.name context)
        ocaml.ocaml_config
        ~exec_dir:(Path.source dir)
        ~plugin:(In_source_dir file)
        ~wrapper
        ~target:generated_dune_file
    in
    let* () =
      let* env = Context.host context >>| Context.installed_env in
      let ocaml = Action.Prog.ok_exn ocaml.ocaml in
      let args =
        [ "-I"; "+compiler-libs"; Path.to_absolute_filename (Path.build wrapper) ]
      in
      Process.run Strict ~display:Quiet ~dir:(Path.source dir) ~env ocaml args
      |> Memo.of_reproducible_fiber
    in
    if not (Path.Untracked.exists (Path.build generated_dune_file))
    then
      User_error.raise
        ~loc:(Loc.in_file (Path.source file))
        [ Pp.textf
            "%s failed to produce a valid dune file."
            (Path.Source.to_string_maybe_quoted file)
        ; Pp.textf "Did you forgot to call [Jbuild_plugin.V*.send]?"
        ];
    Path.build generated_dune_file
    |> Io.Untracked.with_lexbuf_from_file ~f:(Dune_lang.Parser.parse ~mode:Many)
    |> List.rev_append from_parent
    |> parse ~mask ~dir ~file:(Some file) ~project
  ;;
end

module Eval = struct
  type nonrec t =
    | Literal of t
    | Script of Script.t

  open Memo.O

  let context_independent ~mask ~dir project dune_file =
    let file = Dune_file0.path dune_file in
    let static = Dune_file0.get_static_sexp dune_file in
    match Dune_file0.kind dune_file with
    | Ocaml_script ->
      Memo.return
        (Script
           { dir
           ; project
           ; file =
               (* we can't introduce ocaml syntax with [(sudir ..)] *)
               Option.value_exn file
           ; from_parent = static
           })
    | Plain ->
      let+ stanzas = parse static ~mask ~dir ~file ~project in
      Literal stanzas
  ;;

  let eval dune_files (mask : Only_packages.t) =
    let mask = Mask.of_only_packages_mask mask in
    (* CR-rgrinberg: all this evaluation complexity is to share
       some work in multi context builds. Is it worth it? *)
    let+ static, dynamic =
      Appendable_list.to_list dune_files
      |> Memo.parallel_map ~f:(fun (dir, project, dune_file) ->
        let mask = Mask.combine mask (Mask.ignore_promote project) in
        context_independent ~mask ~dir project dune_file)
      >>| List.partition_map ~f:(function
        | Literal x -> Left x
        | Script s -> Right s)
    in
    fun context ->
      let+ dynamic =
        Memo.parallel_map dynamic ~f:(fun script ->
          let mask = Mask.combine mask (Mask.ignore_promote script.project) in
          Script.eval_one ~mask ~context script)
      in
      static @ dynamic
  ;;
end

let eval = Eval.eval
