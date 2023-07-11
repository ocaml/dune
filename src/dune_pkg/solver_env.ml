open! Stdune

module Flag = struct
  module T = struct
    type t =
      [ `With_test
      | `With_doc
      ]

    let to_string = function
      | `With_test -> "with-test"
      | `With_doc -> "with-doc"

    let compare a b = String.compare (to_string a) (to_string b)

    let equal a b = Ordering.is_eq (compare a b)

    let to_dyn t = Dyn.variant (to_string t) []

    let all = [ `With_test; `With_doc ]
  end

  include T

  let of_string_opt s = List.find all ~f:(fun t -> String.equal s (to_string t))

  module Set = struct
    include Set.Of_map (T) (Map.Make (T))

    let all : t = of_list all

    let of_ordered_set ordered_set =
      Dune_lang.Ordered_set_lang.eval ordered_set
        ~parse:(fun ~loc string ->
          match of_string_opt string with
          | Some flag -> flag
          | None ->
            User_error.raise ~loc
              [ Pp.textf "No such flag: %s" (String.maybe_quoted string)
              ; Pp.textf "Valid flags: %s"
                  (String.enumerate_and
                     (List.map T.all ~f:(fun v ->
                          String.maybe_quoted @@ to_string v)))
              ])
        ~eq:T.equal ~standard:T.all
      |> of_list

    let decode =
      let open Dune_lang.Decoder in
      let+ ordered_set = Dune_lang.Ordered_set_lang.decode in
      of_ordered_set ordered_set
  end
end

type t = { flags : Flag.Set.t }

module Fields = struct
  let flags = "flags"
end

(* TODO: combine [Opam.Env] into this record *)
let decode =
  let open Dune_lang.Decoder in
  fields
  @@ let+ flags = field Fields.flags ~default:Flag.Set.all Flag.Set.decode in
     { flags }

let to_dyn t = Dyn.record [ (Fields.flags, Flag.Set.to_dyn t.flags) ]

let equal { flags } t = Flag.Set.equal flags t.flags

let default = { flags = Flag.Set.all }
