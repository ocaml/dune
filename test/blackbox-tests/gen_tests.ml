open Stdune

let sprintf = Printf.sprintf

module Sexp = struct
  let record fields =
    Usexp.List
      (List.map fields ~f:(fun (k, s) ->
         Usexp.List [Usexp.atom k; s]))

  let strings fields =
    Usexp.List (List.map fields ~f:Usexp.atom_or_quoted_string)

  let constr name args =
    Usexp.List [Usexp.atom name; args]
end

let alias ?action name ~deps =
  Sexp.constr "alias"
    (Sexp.record (
       [ "name", Usexp.atom name
       ; "deps", Usexp.List deps
       ] @ (match action with
         | None -> []
         | Some a -> ["action", a])))

module Test = struct
  type t =
    { name           : string
    ; env            : (string * string) option
    ; skip_ocaml     : string option
    ; skip_platforms : Platform.t list
    ; enabled        : bool
    ; js             : bool
    ; external_deps  : bool
    }

  let make ?env ?skip_ocaml ?(skip_platforms=[]) ?(enabled=true) ?(js=false)
        ?(external_deps=false) name =
    { name
    ; env
    ; skip_ocaml
    ; skip_platforms
    ; external_deps
    ; enabled
    ; js
    }

  let pp_sexp fmt t =
    let open Usexp in
    let skip_version =
      match t.skip_ocaml with
      | None -> []
      | Some s -> ["-skip-versions"; s]
    in
    let skip_platforms = Platform.to_cmd t.skip_platforms in
    let action =
      List
        [ atom "chdir"
        ; atom (sprintf "test-cases/%s" t.name)
        ; List
            [ atom "progn"
            ; Sexp.strings (["run"; "${exe:cram.exe}"]
                            @ skip_version
                            @ skip_platforms
                            @ ["-test"; "run.t"])
            ; Sexp.strings ["diff?"; "run.t"; "run.t.corrected"]
            ]

        ]
    in
    let action =
      match t.env with
      | None -> action
      | Some (k, v) ->
        List [ atom "setenv"
             ; atom_or_quoted_string k
             ; atom_or_quoted_string v
             ; action ] in
    alias t.name
      ~deps:(
        [ Sexp.strings ["package"; "dune"]
        ; Sexp.strings [ "files_recursively_in"
                       ; sprintf "test-cases/%s" t.name]
        ]
      ) ~action
    |> Usexp.pp fmt
end

let exclusions =
  let open Test in
  let odoc = make ~external_deps:true ~skip_ocaml:"4.02.3" in
  [ make "js_of_ocaml" ~external_deps:true ~js:true ~env:("NODE", "${bin:node}")
  ; make "github25" ~env:("OCAMLPATH", "./findlib-packages")
  ; odoc "odoc"
  ; odoc "odoc-unique-mlds"
  ; odoc "github717-odoc-index"
  ; odoc "multiple-private-libs"
  ; make "ppx-rewriter" ~skip_ocaml:"4.02.3" ~external_deps:true
  ; make "output-obj" ~skip_platforms:[Mac; Win] ~skip_ocaml:"<4.06.0"
  ; make "github644" ~external_deps:true
  ; make "private-public-overlap" ~external_deps:true
  ; make "reason" ~enabled:false
  ; make "menhir"~external_deps:true
  ; make "utop"~external_deps:true
  ; make "configurator" ~skip_platforms:[Win]
  ; make "github764" ~skip_platforms:[Win]
  ]

let all_tests = lazy (
  Sys.readdir "test-cases"
  |> Array.to_list
  |> List.filter ~f:(fun s -> not (String.contains s '.'))
  |> List.sort ~compare:String.compare
  |> List.map ~f:(fun name ->
    match List.find exclusions ~f:(fun (t : Test.t) -> t.name = name) with
    | None -> Test.make name
    | Some t -> t
  )
)

let pp_group fmt (name, tests) =
  alias name ~deps:(
    (List.map tests ~f:(fun (t : Test.t) ->
       Sexp.strings ["alias"; t.name])))
  |> Usexp.pp fmt

let () =
  let tests = Lazy.force all_tests in
  (* The runtest target has a "specoial" definition. It includes all tests
     except for js and disabled tests *)
  tests |> List.iter ~f:(fun t -> Format.printf "%a@.@." Test.pp_sexp t);
  [ "runtest", (fun (t : Test.t) -> not t.js && t.enabled)
  ; "runtest-no-deps", (fun (t : Test.t) -> not t.external_deps && t.enabled)
  ; "runtest-disabled", (fun (t : Test.t) -> not t.enabled)
  ; "runtest-js", (fun (t : Test.t) -> t.js && t.enabled) ]
  |> List.map ~f:(fun (name, predicate) ->
    (name, List.filter tests ~f:predicate))
  |> Format.pp_print_list
       ~pp_sep:(fun fmt () -> Format.fprintf fmt "@.@.")
       pp_group Format.std_formatter
