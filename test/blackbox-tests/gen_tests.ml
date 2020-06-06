open! Stdune

let sprintf = Printf.sprintf

module Sexp = struct
  let fields fields =
    List.map fields ~f:(fun (k, s) -> Dune_lang.List (Dune_lang.atom k :: s))

  let strings fields =
    Dune_lang.List (List.map fields ~f:Dune_lang.atom_or_quoted_string)

  let constr name args = Dune_lang.List (Dune_lang.atom name :: args)

  let parse s =
    Dune_lang.Parser.parse_string ~fname:"gen_tests.ml" ~mode:Single s
    |> Dune_lang.Ast.remove_locs
end

module Platform = struct
  type t =
    | Win
    | Mac

  open Dune_lang

  let to_string = function
    | Win -> "win"
    | Mac -> "macosx"

  let t t = atom (to_string t)

  let system_var = Sexp.parse "%{ocaml-config:system}"

  let enabled_if = function
    | [] -> None
    | [ x ] -> Some (List [ atom "<>"; system_var; t x ])
    | ps ->
      Some
        (List
           ( atom "and"
           :: List.map ps ~f:(fun p -> List [ atom "<>"; system_var; t p ]) ))
end

let alias ?enabled_if ?action name ~deps =
  let stanza, alias_or_name =
    match action with
    | None -> ("alias", "name")
    | Some _ -> ("rule", "alias")
  in
  Sexp.constr stanza
    (Sexp.fields
       ( [ (alias_or_name, [ Dune_lang.atom name ]); ("deps", deps) ]
       @ ( match action with
         | None -> []
         | Some a -> [ ("action", [ a ]) ] )
       @
       match enabled_if with
       | None -> []
       | Some e -> [ ("enabled_if", [ e ]) ] ))

module Test = struct
  let root_dir = "test-cases"

  type t =
    { path : string
    ; env : (string * Dune_lang.t) option
    ; only_ocaml : (string * string) option
    ; skip_platforms : Platform.t list
    ; enabled : bool
    ; js : bool
    ; coq : bool
    ; external_deps : bool
    ; disable_sandboxing : bool
    ; additional_deps : Dune_lang.t list
    }

  let path_separator =
    let dir_sep = Filename.dir_sep in
    assert (String.length dir_sep == 1);
    dir_sep.[0]

  let alias_name t =
    match String.split t.path ~on:path_separator with
    | [] -> assert false
    | dir :: dirs ->
      assert (dir = root_dir);
      let rec loop acc = function
        | [] -> List.rev acc
        | [ x ] ->
          let acc =
            if x = "run.t" then
              acc
            else
              Filename.chop_suffix x ".t" :: acc
          in
          loop acc []
        | x :: xs -> loop (x :: acc) xs
      in
      String.concat ~sep:"-" (loop [] dirs)

  let filename t = Filename.basename t.path

  let dir t = Filename.dirname t.path

  let make ?env ?only_ocaml ?(skip_platforms = []) ?(enabled = true)
      ?(js = false) ?(coq = false) ?(external_deps = false)
      ?(disable_sandboxing = false) ?(additional_deps = []) path =
    let external_deps = external_deps || coq || js in
    { path
    ; env
    ; only_ocaml
    ; skip_platforms
    ; external_deps
    ; enabled
    ; js
    ; coq
    ; disable_sandboxing
    ; additional_deps
    }

  let make_run_t ?env ?only_ocaml ?skip_platforms ?enabled ?js ?coq
      ?external_deps ?disable_sandboxing ?additional_deps path =
    make ?env ?only_ocaml ?skip_platforms ?enabled ?js ?coq ?external_deps
      ?disable_sandboxing ?additional_deps
      (Filename.concat root_dir (Filename.concat path "run.t"))

  let pp_sexp fmt t =
    let open Dune_lang in
    let enabled_if = Platform.enabled_if t.skip_platforms in
    let dir = dir t in
    (* Make sure we generate paths with forward slashes even on Windows. *)
    let dir =
      String.map dir ~f:(fun c ->
          if c = path_separator then
            '/'
          else
            c)
    in
    let filename = filename t in
    let action =
      List
        [ atom "chdir"
        ; atom dir
        ; List
            [ atom "progn"
            ; Dune_lang.List
                [ atom "run"; atom "dune-cram"; atom "run"; atom filename ]
            ; Sexp.strings [ "diff?"; filename; filename ^ ".corrected" ]
            ]
        ]
    in
    let action =
      match t.env with
      | None -> action
      | Some (k, v) ->
        List [ atom "setenv"; atom_or_quoted_string k; v; action ]
    in
    let enabled_if =
      match t.only_ocaml with
      | None -> enabled_if
      | Some (op, v) ->
        let e = List [ atom op; Sexp.parse "%{ocaml_version}"; atom v ] in
        Some
          ( match enabled_if with
          | None -> e
          | Some e' -> List [ atom "and"; e'; e ] )
    in
    alias (alias_name t) ?enabled_if
      ~deps:
        (List.concat
           [ Sexp.strings [ "package"; "dune" ]
             :: Sexp.strings [ "source_tree"; dir ]
             :: Sexp.strings [ "alias"; "test-deps" ]
             :: t.additional_deps
           ; ( if t.disable_sandboxing then
               [ Sexp.strings [ "sandbox"; "none" ] ]
             else
               [] )
           ])
      ~action
    |> Dune_lang.pp |> Pp.render_ignore_tags fmt
end

module String_map = Map.Make (String)

let exclusions =
  let make = Test.make_run_t in
  let odoc name =
    let name = Filename.concat "odoc" name in
    make ~external_deps:true ~only_ocaml:("<>", "4.02.3") name
  in
  let utop name =
    let name = Filename.concat "utop" name in
    make ~external_deps:true ~only_ocaml:(">=", "4.05.0") name
  in
  let menhir name =
    let name = Filename.concat "menhir" name in
    make ~external_deps:true name
  in
  [ make "js_of_ocaml" ~external_deps:true ~js:true
      ~env:("NODE", Sexp.parse "%{bin:node}")
  ; make "coq" ~coq:true
  ; make "coq/extract" ~coq:true
  ; make "github25" ~env:("OCAMLPATH", Dune_lang.atom "./findlib-packages")
  ; odoc "odoc-simple"
  ; odoc "odoc-package-mld-link"
  ; odoc "odoc-unique-mlds"
  ; odoc "github717-odoc-index"
  ; odoc "multiple-private-libs"
  ; odoc "warnings"
  ; make "cinaps" ~external_deps:true ~enabled:false
  ; make "fdo" ~external_deps:true ~enabled:false ~only_ocaml:(">=", "4.11.0")
  ; make "ppx-rewriter" ~only_ocaml:("<>", "4.02.3") ~external_deps:true
  ; make "cross-compilation" ~external_deps:true
  ; make "dune-ppx-driver-system" ~external_deps:true
  ; make "github1372" ~external_deps:true
  ; make "install-dry-run" ~external_deps:true
  ; make "install-libdir" ~external_deps:true
  ; make "lint" ~external_deps:true
  ; make "ppx-runtime-dependencies" ~external_deps:true
  ; make "foreign-library" ~external_deps:true
  ; make "package-dep" ~external_deps:true
  ; make "merlin/merlin-tests" ~external_deps:true
  ; make "use-meta" ~external_deps:true
  ; make "output-obj" ~skip_platforms:[ Mac; Win ] ~only_ocaml:(">=", "4.06.0")
  ; make "dune-cache/trim"
  ; make "github644" ~external_deps:true
  ; make "private-public-overlap" ~external_deps:true
  ; make "reason" ~external_deps:true
  ; menhir "cmly"
  ; menhir "general"
  ; menhir "general-2.0"
  ; menhir "promote"
  ; utop "utop-simple"
  ; utop "utop-default"
  ; utop "utop-default-implementation"
  ; utop "utop-ppx-rewriters"
  ; make "toplevel-stanza" ~only_ocaml:(">=", "4.05.0") ~external_deps:true
  ; make "github764" ~skip_platforms:[ Win ]
  ; make "gen-opam-install-file" ~external_deps:true
  ; make "scope-ppx-bug" ~external_deps:true
  ; make "findlib-dynload" ~external_deps:true
    (* The next test is disabled as it relies on configured opam swtiches and
       it's hard to get that working properly *)
  ; make "env/envs-and-contexts" ~external_deps:true ~enabled:false
  ; make "env/env-simple" ~only_ocaml:(">=", "4.06.0")
  ; make "env/env-cflags" ~only_ocaml:(">=", "4.06.0")
  ; make "wrapped-transition" ~only_ocaml:(">=", "4.06.0")
  ; make "explicit_js_mode" ~external_deps:true ~js:true
    (* for the following tests sandboxing is disabled because absolute paths end
       up appearing in the output if we sandbox *)
  ; make "env/env-bins" ~disable_sandboxing:true
  ; make "mdx-stanza" ~external_deps:true
  ; make "toplevel-integration" ~external_deps:true
  ]
  |> String_map.of_list_map_exn ~f:(fun (test : Test.t) -> (test.path, test))

let fold_find path ~init ~f =
  let rec dir path acc =
    Sys.readdir path
    |> Array.fold_left ~init:acc ~f:(fun acc file ->
           let path = Filename.concat path file in
           let recurse =
             Sys.is_directory path
             && (Unix.lstat path).st_kind <> S_LNK
             && Filename.basename path <> "_build"
           in
           if recurse then
             dir path acc
           else
             f acc path)
  in
  dir path init

let all_tests =
  lazy
    (let exclusions, tests =
       fold_find Test.root_dir ~init:[] ~f:(fun acc p ->
           if Filename.extension p = ".t" then
             p :: acc
           else
             acc)
       |> List.fold_left ~init:(exclusions, [])
            ~f:(fun (exclusions, tests) path ->
              match String_map.find exclusions path with
              | None -> (exclusions, Test.make path :: tests)
              | Some t -> (String_map.remove exclusions path, t :: tests))
     in
     ( match String_map.values exclusions with
     | [] -> ()
     | not_found_tests ->
       Format.eprintf
         "Failed to discover .t files for the following tests:@.@[<v>%a@]@."
         (Format.pp_print_list (fun fmt (t : Test.t) ->
              Format.fprintf fmt "- %s" t.path))
         not_found_tests );
     List.sort tests ~compare:(fun t1 t2 ->
         String.compare (Test.alias_name t1) (Test.alias_name t2)))

let pp_group fmt (name, tests) =
  alias name
    ~deps:
      (List.map tests ~f:(fun (t : Test.t) ->
           let name = Test.alias_name t in
           Sexp.strings [ "alias"; name ]))
  |> Dune_lang.pp |> Pp.render_ignore_tags fmt

let () =
  let tests = Lazy.force all_tests in
  (* The runtest target has a "special" definition. It includes all tests except
     for js, coq, and disabled tests *)
  tests |> List.iter ~f:(fun t -> Format.printf "%a@.@." Test.pp_sexp t);
  [ ("runtest", fun (t : Test.t) -> (not t.js) && (not t.coq) && t.enabled)
  ; ("runtest-no-deps", fun (t : Test.t) -> (not t.external_deps) && t.enabled)
  ; ("runtest-disabled", fun (t : Test.t) -> not t.enabled)
  ; ("runtest-js", fun (t : Test.t) -> t.js && t.enabled)
  ; ("runtest-coq", fun (t : Test.t) -> t.coq && t.enabled)
  ]
  |> List.map ~f:(fun (name, predicate) ->
         (name, List.filter tests ~f:predicate))
  |> Format.pp_print_list
       ~pp_sep:(fun fmt () -> Format.fprintf fmt "@.@.")
       pp_group Format.std_formatter
