open Import
open Stdune

module Options = struct
  type t = Describe_common.options

  let arg_with_deps =
    let open Arg in
    value & flag
    & info [ "with-deps" ]
        ~doc:"Whether the dependencies between modules should be printed."

  let arg_with_pps =
    let open Arg in
    value & flag
    & info [ "with-pps" ]
        ~doc:
          "Whether the dependencies towards ppx-rewriters (that are called at \
           compile time) should be taken into account."

  let arg_sanitize_for_tests =
    let open Arg in
    value & flag
    & info [ "sanitize-for-tests" ]
        ~doc:
          "Sanitize the absolute paths in workspace items, and the associated \
           UIDs, so that the output is reproducible."

  let arg : t Term.t =
    let+ with_deps = arg_with_deps
    and+ with_pps = arg_with_pps
    and+ sanitize_for_tests_value = arg_sanitize_for_tests in
    Describe_common.sanitize_for_tests := sanitize_for_tests_value;
    { Describe_common.with_deps; with_pps }
end

module Lang = struct
  type t = Dune_lang.Syntax.Version.t

  let arg_conv =
    let parser s =
      match Scanf.sscanf s "%u.%u" (fun a b -> (a, b)) with
      | Ok t -> Ok t
      | Error () -> Error (`Msg "Expected version of the form NNN.NNN.")
    in
    let printer ppf t =
      Stdlib.Format.fprintf ppf "%s" (Dune_lang.Syntax.Version.to_string t)
    in
    Arg.conv ~docv:"VERSION" (parser, printer)

  let arg : t Term.t =
    Term.ret
    @@ let+ v =
         Arg.(
           value
           & opt arg_conv (0, 1)
           & info [ "lang" ] ~docv:"VERSION"
               ~doc:"Behave the same as this version of Dune.")
       in
       if v = (0, 1) then `Ok v
       else
         let msg =
           let pp =
             "Only --lang 0.1 is available at the moment as this command is \
              not yet stabilised. If you would like to release a software that \
              relies on the output of 'dune describe', please open a ticket on \
              https://github.com/ocaml/dune." |> Pp.text
           in
           Stdlib.Format.asprintf "%a" Pp.to_fmt pp
         in
         `Error (true, msg)
end

let print_as_sexp dyn =
  let rec dune_lang_of_sexp : Sexp.t -> Dune_lang.t = function
    | Atom s -> Dune_lang.atom_or_quoted_string s
    | List l -> List (List.map l ~f:dune_lang_of_sexp)
  in
  let cst =
    dyn |> Sexp.of_dyn |> dune_lang_of_sexp
    |> Dune_lang.Ast.add_loc ~loc:Loc.none
    |> Dune_lang.Cst.concrete
  in
  let version = Dune_lang.Syntax.greatest_supported_version Stanza.syntax in
  Pp.to_fmt Stdlib.Format.std_formatter
    (Dune_lang.Format.pp_top_sexps ~version [ cst ])

(** The following module is responsible sanitizing the output of
    [dune describe workspace], so that the absolute paths and the UIDs that
    depend on them are stable for tests. These paths may differ, depending on
    the machine they are run on. *)
module Sanitize_for_tests = struct
  module Workspace = struct
    (** Sanitizes a workspace description, by renaming non-reproducible UIDs and
        paths *)
    let really_sanitize (context : Context.t) items =
      let rename_path =
        let findlib_paths =
          context.findlib_paths |> List.map ~f:Path.to_string
        in
        function
        (* we have found a path for OCaml's root: let's define the renaming
           function *)
        | Path.External ext_path as path -> (
          match
            List.find_map findlib_paths ~f:(fun prefix ->
                (* if the path to rename is an external path, try to find the
                   OCaml root inside, and replace it with a fixed string *)
                let s = Path.External.to_string ext_path in
                match String.drop_prefix ~prefix s with
                | None -> None
                | Some s' ->
                  (* we have found the OCaml root path: let's replace it with a
                     constant string *)
                  Some
                    (Path.external_
                    @@ Path.External.of_string
                         Filename.(concat dir_sep @@ concat "FINDLIB" s')))
          with
          | None -> path
          | Some p -> p)
        | Path.In_source_tree p ->
          (* Replace the workspace root with a fixed string *)
          let p =
            let new_root = Filename.(concat dir_sep "WORKSPACE_ROOT") in
            if Path.Source.is_root p then new_root
            else Filename.(concat new_root (Path.Source.to_string p))
          in
          Path.external_ (Path.External.of_string p)
        | path ->
          (* Otherwise, it should not be changed *)
          path
      in
      (* now, we rename the UIDs in the [requires] field , while reversing the
         list of items, so that we get back the original ordering *)
      List.map ~f:(Describe_common.Descr.Item.map_path ~f:rename_path) items

    (** Sanitizes a workspace description when options ask to do so, or performs
        no change at all otherwise *)
    let sanitize context items =
      if !Describe_common.sanitize_for_tests then really_sanitize context items
      else items
  end
end

let term : unit Term.t =
  let+ common = Common.term
  and+ what =
    Arg.(
      value & pos_all string []
      & info [] ~docv:"DIRS"
          ~doc:
            "prints a description of the workspace's structure. If some \
             directories DIRS are provided, then only those directories of the \
             workspace are considered.")
  and+ context_name = Common.context_arg ~doc:"Build context to use."
  and+ format = Describe_common.Format.arg
  and+ lang = Lang.arg
  and+ options = Options.arg in
  let config = Common.init common in
  let dirs =
    let args = "workspace" :: what in
    let parse =
      Dune_lang.Syntax.set Stanza.syntax (Active lang)
      @@
      let open Dune_lang.Decoder in
      fields @@ field "workspace"
      @@ let+ dirs = repeat relative_file in
         (* [None] means that all directories should be accepted,
            whereas [Some l] means that only the directories in the
            list [l] should be accepted. The checks on whether the
            paths exist and whether they are directories are performed
            later in the [describe] function. *)
         let dirs = if List.is_empty dirs then None else Some dirs in
         dirs
    in
    let ast =
      Dune_lang.Ast.add_loc ~loc:Loc.none
        (List (List.map args ~f:Dune_lang.atom_or_quoted_string))
    in
    Dune_lang.Decoder.parse parse Univ_map.empty ast
  in
  Scheduler.go ~common ~config @@ fun () ->
  let open Fiber.O in
  let* setup = Import.Main.setup () in
  let* setup = Memo.run setup in
  let super_context = Import.Main.find_scontext_exn setup ~name:context_name in
  let+ res =
    Build_system.run_exn @@ fun () ->
    let context = Super_context.context super_context in
    let open Memo.O in
    let* dirs =
      (* prefix directories with the workspace root, so that the
         command also works correctly when it is run from a
         subdirectory *)
      Memo.Option.map dirs
        ~f:
          (Memo.List.map ~f:(fun dir ->
               let p =
                 Path.Source.(relative root) (Common.prefix_target common dir)
               in
               let s = Path.source p in
               if not @@ Path.exists s then
                 User_error.raise
                   [ Pp.textf "No such file or directory: %s" (Path.to_string s)
                   ];
               if not @@ Path.is_directory s then
                 User_error.raise
                   [ Pp.textf "File exists, but is not a directory: %s"
                       (Path.to_string s)
                   ];
               Memo.return p))
    in
    Describe_common.Crawl.workspace options dirs setup context
    >>| Sanitize_for_tests.Workspace.sanitize context
    >>| Describe_common.Descr.Workspace.to_dyn options
  in
  match format with
  | Describe_common.Format.Csexp -> Csexp.to_channel stdout (Sexp.of_dyn res)
  | Sexp -> print_as_sexp res

let command =
  let doc =
    "Print a description of the workspace's structure. If some directories \
     DIRS are provided, then only those directories of the workspace are \
     considered."
  in
  let info = Cmd.info ~doc "workspace" in
  Cmd.v info term
