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

  let encode t =
    let open Dune_lang.Encoder in
    string (to_string t)

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

    let encode t =
      let open Dune_lang.Encoder in
      list encode (to_list t)
  end
end

module Sys_var = struct
  module T = struct
    type t =
      [ `Arch
      | `Os
      | `Os_version
      | `Os_distribution
      | `Os_family
      ]

    let to_string = function
      | `Arch -> "arch"
      | `Os -> "os"
      | `Os_version -> "os-version"
      | `Os_distribution -> "os-distribution"
      | `Os_family -> "os-family"

    let compare a b = String.compare (to_string a) (to_string b)

    let to_dyn t = Dyn.string (to_string t)

    let all = [ `Arch; `Os; `Os_version; `Os_distribution; `Os_family ]
  end

  include T

  let of_string_opt s = List.find all ~f:(fun t -> String.equal s (to_string t))

  let decode =
    let open Dune_lang.Decoder in
    let+ loc, name = located string in
    match of_string_opt name with
    | Some t -> t
    | None ->
      User_error.raise ~loc
        [ Pp.textf "No such sys variable: %s" (String.maybe_quoted name)
        ; Pp.textf "Valid variables: %s"
            (String.enumerate_and
               (List.map T.all ~f:(fun v -> String.maybe_quoted @@ to_string v)))
        ]

  let encode t =
    let open Dune_lang.Encoder in
    string (to_string t)

  module Map = Map.Make (T)

  module Bindings = struct
    type t = string Map.t

    let empty = Map.empty

    let to_dyn = Map.to_dyn Dyn.string

    let equal = Map.equal ~equal:String.equal

    let set = Map.set

    let get = Map.find

    let decode =
      let open Dune_lang.Decoder in
      let+ loc, bindings = located (repeat (pair decode string)) in
      match Map.of_list bindings with
      | Ok t -> t
      | Error (duplicate_key, a, b) ->
        User_error.raise ~loc
          [ Pp.textf "Duplicate entries for sys variable %s (%s, %s)"
              (String.maybe_quoted (to_string duplicate_key))
              (String.maybe_quoted a) (String.maybe_quoted b)
          ]

    let encode t =
      let open Dune_lang.Encoder in
      list (pair encode string) (Map.to_list t)

    type merge_error =
      [ `Var_in_both_with_different_values of T.t * string * string ]

    let merge a b =
      let b_without_dup_values =
        Map.filteri b ~f:(fun var b_value ->
            match get a var with
            | None -> true
            | Some a_value -> not (String.equal a_value b_value))
      in
      Map.of_list (Map.to_list a @ Map.to_list b_without_dup_values)
      |> Result.map_error ~f:(fun (key, a, b) ->
             assert (not (String.equal a b));
             `Var_in_both_with_different_values (key, a, b))
  end
end

type t =
  { flags : Flag.Set.t
  ; sys : Sys_var.Bindings.t
  }

let default = { flags = Flag.Set.all; sys = Sys_var.Bindings.empty }

module Fields = struct
  let flags = "flags"

  let sys = "sys"
end

let decode =
  let open Dune_lang.Decoder in
  fields
  @@ let+ flags = field Fields.flags ~default:Flag.Set.all Flag.Set.decode
     and+ sys = field Fields.sys ~default:default.sys Sys_var.Bindings.decode in
     { flags; sys }

let encode { flags; sys } =
  let open Dune_lang.Encoder in
  record
    [ (Fields.flags, Flag.Set.encode flags)
    ; (Fields.sys, Sys_var.Bindings.encode sys)
    ]

let to_dyn { flags; sys } =
  Dyn.record
    [ (Fields.flags, Flag.Set.to_dyn flags)
    ; (Fields.sys, Sys_var.Bindings.to_dyn sys)
    ]

let equal { flags; sys } t =
  Flag.Set.equal flags t.flags && Sys_var.Bindings.equal sys t.sys
