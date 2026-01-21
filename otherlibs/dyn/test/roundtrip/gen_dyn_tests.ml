let int_cases = [ Dyn.int 42; Dyn.int (-1); Dyn.int 0; Dyn.int max_int; Dyn.int min_int ]

let int32_cases =
  [ Dyn.int32 42l; Dyn.int32 (-1l); Dyn.int32 Int32.max_int; Dyn.int32 Int32.min_int ]
;;

let int64_cases =
  [ Dyn.int64 42L; Dyn.int64 (-1L); Dyn.int64 Int64.max_int; Dyn.int64 Int64.min_int ]
;;

let nativeint_cases = [ Dyn.nativeint 42n; Dyn.nativeint (-1n) ]

let string_cases =
  [ Dyn.string "hello"
  ; Dyn.string ""
  ; Dyn.string "with\nnewline"
  ; Dyn.string "with\ttab"
  ; Dyn.string "with \"quotes\""
  ; Dyn.string "with 'single quotes'"
  ; Dyn.string "with\\backslash"
  ; Dyn.string "\x00\x01\x02"
  ]
;;

(* CR-someday Alizter: bytes prints as string literal, needs Bytes.of_string wrapper *)
let _bytes_cases =
  [ Dyn.Bytes (Bytes.of_string "hello")
  ; Dyn.Bytes (Bytes.of_string "")
  ; Dyn.Bytes (Bytes.of_string "with\nnewline")
  ]
;;

let char_cases =
  [ Dyn.char 'a'
  ; Dyn.char '\n'
  ; Dyn.char '\t'
  ; Dyn.char '\r'
  ; Dyn.char '\''
  ; Dyn.char '"'
  ; Dyn.char '\\'
  ; Dyn.char '\x00'
  ]
;;

let float_cases =
  [ Dyn.float 3.14
  ; Dyn.float 0.0
  ; Dyn.float (-0.0)
  ; Dyn.float (-1.5)
  ; Dyn.float Float.max_float
  ; Dyn.float Float.min_float
  ; Dyn.float 1e-300
  ; Dyn.float 1e300
  ]
;;

let float_special_cases = [ Dyn.float infinity; Dyn.float neg_infinity; Dyn.float nan ]
let bool_cases = [ Dyn.bool true; Dyn.bool false ]
let unit_cases = [ Dyn.unit () ]

let list_cases =
  [ Dyn.list Dyn.int [ 1; 2; 3 ]
  ; Dyn.list Dyn.int []
  ; Dyn.list Dyn.string [ "a"; "b" ]
  ; Dyn.list Dyn.bool [ true; false; true ]
  ]
;;

let array_cases =
  [ Dyn.array Dyn.int [| 1; 2; 3 |]
  ; Dyn.array Dyn.int [||]
  ; Dyn.array Dyn.string [| "x"; "y" |]
  ]
;;

let option_cases =
  [ Dyn.option Dyn.int None
  ; Dyn.option Dyn.int (Some 42)
  ; Dyn.option Dyn.string (Some "hello")
  ; Dyn.option Dyn.string None
  ]
;;

let tuple_cases =
  [ Dyn.pair Dyn.int Dyn.string (1, "x")
  ; Dyn.triple Dyn.int Dyn.string Dyn.bool (1, "x", true)
  ; Dyn.pair Dyn.unit Dyn.unit ((), ())
  ]
;;

let nested_cases =
  [ Dyn.list (Dyn.list Dyn.int) [ [ 1; 2 ]; [ 3 ] ]
  ; Dyn.list (Dyn.list Dyn.int) [ []; [ 1 ]; [ 2; 3 ] ]
  ; Dyn.option (Dyn.option Dyn.int) (Some (Some 1))
  ; Dyn.option (Dyn.option Dyn.int) (Some None)
  ; Dyn.option (Dyn.option Dyn.int) None
  ; Dyn.option (Dyn.list Dyn.int) (Some [ 1; 2; 3 ])
  ; Dyn.option (Dyn.list Dyn.int) (Some [])
  ; Dyn.list (Dyn.option Dyn.int) [ Some 1; None; Some 2 ]
  ; Dyn.list (Dyn.pair Dyn.int Dyn.string) [ 1, "a"; 2, "b" ]
  ; Dyn.pair (Dyn.list Dyn.int) (Dyn.option Dyn.string) ([ 1; 2 ], Some "x")
  ; Dyn.array (Dyn.option Dyn.int) [| Some 1; None |]
  ]
;;

let deeply_nested_cases =
  [ Dyn.list (Dyn.list (Dyn.list Dyn.int)) [ [ [ 1 ] ]; [ [ 2; 3 ]; [ 4 ] ] ]
  ; Dyn.option (Dyn.option (Dyn.option Dyn.int)) (Some (Some (Some 42)))
  ; Dyn.pair
      (Dyn.pair Dyn.int Dyn.int)
      (Dyn.pair Dyn.string Dyn.string)
      ((1, 2), ("a", "b"))
  ]
;;

(* Records need type definitions before use *)
let record_cases =
  [ [ "x", Dyn.int 1 ]
  ; [ "x", Dyn.int 1; "y", Dyn.string "hello" ]
  ; [ "name", Dyn.string "foo"; "value", Dyn.bool true ]
  ]
;;

(* Variants need type definitions - each case is (constructor_name, args) list for the type *)
let variant_cases =
  [ [ "Foo", [] ], "Foo", []
  ; [ "Bar", [ Dyn.int 0 ] ], "Bar", [ Dyn.int 42 ]
  ; [ "Baz", [ Dyn.int 0; Dyn.string "" ] ], "Baz", [ Dyn.int 1; Dyn.string "x" ]
  ]
;;

let all_cases =
  List.concat
    [ int_cases
    ; int32_cases
    ; int64_cases
    ; nativeint_cases
    ; string_cases
    ; char_cases
    ; float_cases
    ; float_special_cases
    ; bool_cases
    ; unit_cases
    ; list_cases
    ; array_cases
    ; option_cases
    ; tuple_cases
    ; nested_cases
    ; deeply_nested_cases
    ]
;;

let rec pp_type =
  let open Dyn in
  function
  | Opaque -> Pp.verbatim "Dyn.t"
  | Unit -> Pp.verbatim "unit"
  | Int _ -> Pp.verbatim "int"
  | Int32 _ -> Pp.verbatim "int32"
  | Int64 _ -> Pp.verbatim "int64"
  | Nativeint _ -> Pp.verbatim "nativeint"
  | Bool _ -> Pp.verbatim "bool"
  | String _ -> Pp.verbatim "string"
  | Bytes _ -> Pp.verbatim "bytes"
  | Char _ -> Pp.verbatim "char"
  | Float _ -> Pp.verbatim "float"
  | Option None -> Pp.verbatim "_ option"
  | Option (Some x) -> Pp.concat ~sep:Pp.space [ pp_type x; Pp.verbatim "option" ]
  | List [] -> Pp.verbatim "_ list"
  | List (x :: _) -> Pp.concat ~sep:Pp.space [ pp_type x; Pp.verbatim "list" ]
  | Array [||] -> Pp.verbatim "_ array"
  | Array a -> Pp.concat ~sep:Pp.space [ pp_type a.(0); Pp.verbatim "array" ]
  | Tuple xs ->
    Pp.concat
      [ Pp.char '('
      ; Pp.concat_map
          ~sep:(Pp.concat [ Pp.space; Pp.verbatim "*"; Pp.space ])
          xs
          ~f:pp_type
      ; Pp.char ')'
      ]
  | Record _ -> assert false
  | Variant _ -> assert false
  | Map _ -> Pp.verbatim "Dyn.t"
  | Set _ -> Pp.verbatim "Dyn.t"
;;

let pp_record_type_def ~name fields =
  Pp.hovbox
    (Pp.concat
       [ Pp.verbatim "type"
       ; Pp.space
       ; Pp.verbatim name
       ; Pp.space
       ; Pp.verbatim "="
       ; Pp.space
       ; Pp.verbatim "{"
       ; Pp.space
       ; Pp.concat_map ~sep:(Pp.verbatim "; ") fields ~f:(fun (field_name, v) ->
           Pp.concat
             [ Pp.verbatim field_name; Pp.space; Pp.verbatim ":"; Pp.space; pp_type v ])
       ; Pp.space
       ; Pp.verbatim "}"
       ])
;;

let pp_variant_type_def ~name constructors =
  let pp_constructor (ctor_name, args) =
    match args with
    | [] -> Pp.verbatim ctor_name
    | _ ->
      Pp.concat
        [ Pp.verbatim ctor_name
        ; Pp.space
        ; Pp.verbatim "of"
        ; Pp.space
        ; Pp.concat_map
            ~sep:(Pp.concat [ Pp.space; Pp.verbatim "*"; Pp.space ])
            args
            ~f:pp_type
        ]
  in
  Pp.hovbox
    (Pp.concat
       [ Pp.verbatim "type"
       ; Pp.space
       ; Pp.verbatim name
       ; Pp.space
       ; Pp.verbatim "="
       ; Pp.space
       ; Pp.concat_map
           ~sep:(Pp.concat [ Pp.space; Pp.verbatim "|"; Pp.space ])
           constructors
           ~f:pp_constructor
       ])
;;

let pp_binding dyn =
  Pp.hovbox
    (Pp.concat
       [ Pp.verbatim "let"
       ; Pp.space
       ; Pp.verbatim "_"
       ; Pp.space
       ; Pp.verbatim ":"
       ; Pp.space
       ; pp_type dyn
       ; Pp.space
       ; Pp.verbatim "="
       ; Pp.space
       ; Dyn.pp dyn
       ])
;;

let pp_record_binding ~type_name fields =
  Pp.hovbox
    (Pp.concat
       [ Pp.verbatim "let"
       ; Pp.space
       ; Pp.verbatim "_"
       ; Pp.space
       ; Pp.verbatim ":"
       ; Pp.space
       ; Pp.verbatim type_name
       ; Pp.space
       ; Pp.verbatim "="
       ; Pp.space
       ; Dyn.pp (Dyn.record fields)
       ])
;;

let pp_variant_binding ~type_name ctor_name args =
  Pp.hovbox
    (Pp.concat
       [ Pp.verbatim "let"
       ; Pp.space
       ; Pp.verbatim "_"
       ; Pp.space
       ; Pp.verbatim ":"
       ; Pp.space
       ; Pp.verbatim type_name
       ; Pp.space
       ; Pp.verbatim "="
       ; Pp.space
       ; Dyn.pp (Dyn.variant ctor_name args)
       ])
;;

let () =
  let simple_bindings = Pp.concat_map ~sep:Pp.cut all_cases ~f:pp_binding in
  let record_bindings =
    Pp.concat_mapi ~sep:Pp.cut record_cases ~f:(fun i fields ->
      let type_name = Printf.sprintf "record_%d" i in
      Pp.concat
        [ pp_record_type_def ~name:type_name fields
        ; Pp.cut
        ; pp_record_binding ~type_name fields
        ])
  in
  let variant_bindings =
    Pp.concat_mapi ~sep:Pp.cut variant_cases ~f:(fun i (constructors, ctor_name, args) ->
      let type_name = Printf.sprintf "variant_%d" i in
      Pp.concat
        [ pp_variant_type_def ~name:type_name constructors
        ; Pp.cut
        ; pp_variant_binding ~type_name ctor_name args
        ])
  in
  let pp =
    Pp.vbox
      (Pp.concat [ simple_bindings; Pp.cut; record_bindings; Pp.cut; variant_bindings ])
  in
  Format.printf "%a@." Pp.to_fmt pp
;;
