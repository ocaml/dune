open Import

type t =
  { name : string
  ; args : t list
  }

let of_string ~sep str =
  let rec count_empty total = function
    | "" :: rest -> count_empty (total + 1) rest
    | rest -> total, rest
  in
  let parts = String.split_on_char ~sep str in
  let rec apply depth { name; args } arg =
    let args =
      match depth, args with
      | 0, _ -> arg :: args
      | _, hd :: args -> apply (depth - 1) hd arg :: args
      | _ -> assert false
    in
    { name; args }
  in
  let t, rest =
    match parts with
    | [] -> assert false
    | name :: rest -> { name; args = [] }, rest
  in
  let rec go t rest =
    let depth, rest = count_empty 0 rest in
    match rest with
    | [] ->
      assert (Int.equal depth 0);
      t
    | name :: rest ->
      let t = apply depth t { name; args = [] } in
      go t rest
  in
  go t rest
;;

let to_string ~sep t =
  let sep = String.make 1 sep in
  let buf = Buffer.create 16 in
  let rec go apply_sep { name; args } =
    Buffer.add_string buf name;
    let apply_sep' = sep ^ apply_sep in
    List.iter args ~f:(fun arg ->
      Buffer.add_string buf apply_sep;
      go apply_sep' arg)
  in
  go sep t;
  Buffer.contents buf
;;

let to_module_name t =
  let applied_name = to_string ~sep:'-' t in
  let module_name = Module_name.of_string_allow_invalid (Loc.none, applied_name) in
  Module_name.Unique.of_name_assuming_needs_no_mangling module_name
;;

let of_string str = of_string ~sep:'!' str
let to_string t = to_string ~sep:'!' t
