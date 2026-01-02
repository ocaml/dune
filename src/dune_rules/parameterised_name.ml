open Import

type 'a t =
  { name : 'a
  ; args : 'a t list
  }

let of_string ~sep name_of_string str =
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
  let leaf name = { name = name_of_string name; args = [] } in
  let t, rest =
    match parts with
    | [] -> assert false
    | name :: rest -> leaf name, rest
  in
  let rec go t rest =
    let depth, rest = count_empty 0 rest in
    match rest with
    | [] ->
      assert (Int.equal depth 0);
      t
    | name :: rest ->
      let t = apply depth t (leaf name) in
      go t rest
  in
  go t rest
;;

let to_string ~sep string_of_name t =
  let sep = String.make 1 sep in
  let buf = Buffer.create 16 in
  let rec go apply_sep { name; args } =
    Buffer.add_string buf (string_of_name name);
    let apply_sep' = sep ^ apply_sep in
    List.iter args ~f:(fun arg ->
      Buffer.add_string buf apply_sep;
      go apply_sep' arg)
  in
  go sep t;
  Buffer.contents buf
;;

let to_module_name t =
  let applied_name = to_string ~sep:'-' Module_name.to_string t in
  let module_name = Module_name.of_string_allow_invalid (Loc.none, applied_name) in
  Module_name.Unique.of_name_assuming_needs_no_mangling
    (Module_name.Unchecked.allow_invalid module_name)
;;

let to_string = to_string ~sep:'!' Lib_name.to_string
let of_string = of_string ~sep:'!' Lib_name.of_string

module Scope = struct
  type t = Path.Source.t option

  let equal = Option.equal Path.Source.equal

  let to_string = function
    | None -> ""
    | Some path -> Path.Source.to_string path
  ;;

  let reverse_table : (Digest.t, t) Table.t = Table.create (module Digest) 128

  let encode t =
    let key = Digest.generic (to_string t) in
    match Table.find reverse_table key with
    | None ->
      Table.set reverse_table key t;
      key
    | Some t' ->
      if equal t t'
      then key
      else
        User_error.raise
          [ Pp.textf "Hash collision between two scopes:"
          ; Pp.textf "- cache : %s" (to_string t')
          ; Pp.textf "- fetch : %s" (to_string t)
          ]
  ;;

  let encode t = Digest.to_string (encode t)

  let decode digest =
    match Digest.from_hex digest with
    | None -> User_error.raise [ Pp.textf "Scope %S is not a valid digest." digest ]
    | Some key ->
      (match Table.find reverse_table key with
       | Some t -> t
       | None -> User_error.raise [ Pp.textf "Unknown scope %S." digest ])
  ;;
end
