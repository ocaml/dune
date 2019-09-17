open Stdune

module Select = struct
  type choice =
    { required : Lib_name.Set.t
    ; forbidden : Lib_name.Set.t
    ; file : string
    }

  let dyn_of_choice { required; forbidden; file } =
    let open Dyn.Encoder in
    record
      [ ("required", Lib_name.Set.to_dyn required)
      ; ("forbidden", Lib_name.Set.to_dyn forbidden)
      ; ("file", string file)
      ]

  type t =
    { result_fn : string
    ; choices : choice list
    ; loc : Loc.t
    }

  let to_dyn { result_fn; choices; loc = _ } =
    let open Dyn.Encoder in
    record
      [ ("result_fn", string result_fn)
      ; ("choices", list dyn_of_choice choices)
      ]
end

type t =
  | Direct of (Loc.t * Lib_name.t)
  | Re_export of (Loc.t * Lib_name.t)
  | Select of Select.t

let to_dyn =
  let open Dyn.Encoder in
  function
  | Direct (_, name) -> Lib_name.to_dyn name
  | Re_export (_, name) -> constr "re_export" [ Lib_name.to_dyn name ]
  | Select s -> constr "select" [ Select.to_dyn s ]

let direct x = Direct x

let re_export x = Re_export x

let to_lib_names = function
  | Direct (_, s)
   |Re_export (_, s) ->
    [ s ]
  | Select s ->
    List.fold_left s.choices ~init:Lib_name.Set.empty
      ~f:(fun acc (x : Select.choice) ->
        Lib_name.Set.union acc (Lib_name.Set.union x.required x.forbidden))
    |> Lib_name.Set.to_list

let choice =
  let open Dune_lang.Decoder in
  enter
    (let+ loc = loc
     and+ preds, file =
       until_keyword "->"
         ~before:
           (let+ s = string
            and+ loc = loc in
            let len = String.length s in
            if len > 0 && s.[0] = '!' then
              Right (Lib_name.of_string_exn ~loc:(Some loc) (String.drop s 1))
            else
              Left (Lib_name.of_string_exn ~loc:(Some loc) s))
         ~after:file
     in
     match file with
     | None ->
       User_error.raise ~loc
         [ Pp.textf "(<[!]libraries>... -> <file>) expected" ]
     | Some file ->
       let rec loop required forbidden = function
         | [] ->
           let common = Lib_name.Set.inter required forbidden in
           Option.iter (Lib_name.Set.choose common) ~f:(fun name ->
               User_error.raise ~loc
                 [ Pp.textf
                     "library %S is both required and forbidden in this clause"
                     (Lib_name.to_string name)
                 ]);
           { Select.required; forbidden; file }
         | Left s :: l -> loop (Lib_name.Set.add required s) forbidden l
         | Right s :: l -> loop required (Lib_name.Set.add forbidden s) l
       in
       loop Lib_name.Set.empty Lib_name.Set.empty preds)

let decode =
  let open Dune_lang.Decoder in
  if_list
    ~then_:
      (enter
         (let* loc = loc in
          let* constr = string in
          match constr with
          | "re_export" ->
            let+ () = Dune_lang.Syntax.since Stanza.syntax (2, 0)
            and+ loc, name = located Lib_name.decode in
            Re_export (loc, name)
          | "select" ->
            let+ result_fn = file
            and+ () = keyword "from"
            and+ choices = repeat choice in
            Select { result_fn; choices; loc }
          | _ -> assert false))
    ~else_:
      (let+ loc, name = located Lib_name.decode in
       Direct (loc, name))

let encode =
  let open Dune_lang.Encoder in
  function
  | Direct (_, name) -> Lib_name.encode name
  | Re_export (_, name) -> constr "re_export" Lib_name.encode name
  | Select select ->
    Code_error.raise "Lib_dep.encode: cannot encode select"
      [ ("select", Select.to_dyn select) ]

module L = struct
  let field_encode t ~name =
    let open Dune_lang.Encoder in
    field_l name encode t
end
