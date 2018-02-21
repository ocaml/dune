open! Import
open Sexp.Of_sexp

type t = ..

let parse = ref (fun acc -> return acc)

let registered = ref Sub_system_name.Set.empty

let register ~name ?short ~of_sexp () =
  let s = Sub_system_name.to_string name in
  if Sub_system_name.Set.mem name !registered then
    Sexp.code_error "Sub_system_info.register: already registered"
      [ "name", Sexp.To_sexp.atom s ];
  registered := Sub_system_name.Set.add name !registered;
  let p = !parse in
  parse := (fun acc ->
    field_o s of_sexp ?short >>= fun x ->
    let acc =
      match x with
      | None -> acc
      | Some x -> Sub_system_name.Map.add acc ~key:name ~data:x
    in
    p acc)

let parse () = !parse Sub_system_name.Map.empty
