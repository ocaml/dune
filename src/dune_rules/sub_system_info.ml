open Import
open Dune_lang.Decoder

type t = ..

let equal (x : t) (y : t) = phys_equal x y

type sub_system = t = ..

module type S = sig
  type t
  type sub_system += T of t

  val name : Sub_system_name.t
  val loc : t -> Loc.t
  val syntax : Dune_lang.Syntax.t
  val decode : t Dune_lang.Decoder.t
  val encode : t -> Dune_lang.Syntax.Version.t * Dune_lang.t list
end

(* This mutable table is safe under the assumption that subsystems are
   registered at the top level, which is currently true. *)
let all = Table.create (module Sub_system_name) 16

(* For parsing config files in the workspace *)
let record_parser = ref return

module Register (M : S) : sig end = struct
  open M

  let () =
    match Table.find all name with
    | Some _ ->
      Code_error.raise
        "Sub_system_info.register: already registered"
        [ "name", Dyn.string (Sub_system_name.to_string name) ]
    | None ->
      Table.set all name (module M : S);
      let p = !record_parser in
      let name_s = Sub_system_name.to_string name in
      record_parser
      := fun acc ->
           field_o name_s decode
           >>= (function
            | None -> p acc
            | Some x ->
              let acc = Sub_system_name.Map.set acc name (T x) in
              p acc)
  ;;
end

let record_parser =
  let* () = return () in
  !record_parser Sub_system_name.Map.empty
;;

let get name = Table.find_exn all name
