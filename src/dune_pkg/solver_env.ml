open! Stdune

module Variable = struct
  module Flag = struct
    module T = struct
      type t =
        [ `With_test
        | `With_doc
        ]

      let to_string = function
        | `With_test -> "with-test"
        | `With_doc -> "with-doc"
      ;;

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
        Dune_lang.Ordered_set_lang.eval
          ordered_set
          ~parse:(fun ~loc string ->
            match of_string_opt string with
            | Some flag -> flag
            | None ->
              User_error.raise
                ~loc
                [ Pp.textf "No such flag: %s" (String.maybe_quoted string)
                ; Pp.textf
                    "Valid flags: %s"
                    (String.enumerate_and
                       (List.map T.all ~f:(fun v -> String.maybe_quoted @@ to_string v)))
                ])
          ~eq:T.equal
          ~standard:T.all
        |> of_list
      ;;

      let decode =
        let open Dune_lang.Decoder in
        let+ ordered_set = Dune_lang.Ordered_set_lang.decode in
        of_ordered_set ordered_set
      ;;

      let pp t =
        to_list all
        |> Pp.enumerate ~f:(fun flag -> Pp.textf "%s = %b" (to_string flag) (mem t flag))
      ;;
    end
  end

  module Sys = struct
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
      ;;

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
        User_error.raise
          ~loc
          [ Pp.textf "No such sys variable: %s" (String.maybe_quoted name)
          ; Pp.textf
              "Valid variables: %s"
              (String.enumerate_and
                 (List.map T.all ~f:(fun v -> String.maybe_quoted @@ to_string v)))
          ]
    ;;

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
          User_error.raise
            ~loc
            [ Pp.textf
                "Duplicate entries for sys variable %s (%s, %s)"
                (String.maybe_quoted (to_string duplicate_key))
                (String.maybe_quoted a)
                (String.maybe_quoted b)
            ]
      ;;

      type union_error = [ `Var_in_both_with_different_values of T.t * string * string ]

      exception E of union_error

      let union a b =
        try
          Map.union a b ~f:(fun common_key a_value b_value ->
            if String.equal a_value b_value
            then Some a_value
            else
              raise
                (E (`Var_in_both_with_different_values (common_key, a_value, b_value))))
          |> Result.ok
        with
        | E union_error -> Error union_error
      ;;

      let pp t =
        Pp.enumerate all ~f:(fun variable ->
          match Map.find t variable with
          | Some value ->
            Pp.textf "%s = %s" (to_string variable) (String.maybe_quoted value)
          | None -> Pp.textf "%s (unset)" (to_string variable))
      ;;
    end
  end

  module Const = struct
    type t = [ `Opam_version ]

    let to_string = function
      | `Opam_version -> "opam-version"
    ;;

    let all = [ `Opam_version ]
    let of_string_opt s = List.find all ~f:(fun t -> String.equal s (to_string t))

    module Bindings = struct
      type t = { opam_version : string }

      let to_dyn { opam_version } = Dyn.(record [ "opam_version", string opam_version ])
      let equal { opam_version } t = String.equal opam_version t.opam_version

      let get { opam_version } = function
        | `Opam_version -> opam_version
      ;;

      let pp { opam_version } =
        Pp.enumerate
          [ `Opam_version, opam_version ]
          ~f:(fun (variable, value) -> Pp.textf "%s = %s" (to_string variable) value)
      ;;
    end

    let bindings = { Bindings.opam_version = OpamVersion.to_string OpamVersion.current }
  end

  type t =
    | Flag of Flag.t
    | Sys of Sys.t
    | Const of Const.t

  let of_string_opt string =
    match Flag.of_string_opt string with
    | Some flag -> Some (Flag flag)
    | None ->
      (match Sys.of_string_opt string with
       | Some sys -> Some (Sys sys)
       | None ->
         (match Const.of_string_opt string with
          | Some const -> Some (Const const)
          | None -> None))
  ;;
end

type t =
  { flags : Variable.Flag.Set.t
  ; sys : Variable.Sys.Bindings.t
  ; const : Variable.Const.Bindings.t
  }

module Fields = struct
  let flags = "flags"
  let sys = "sys"
  let const = "const"
end

let default =
  { flags = Variable.Flag.Set.all
  ; sys = Variable.Sys.Bindings.empty
  ; const = Variable.Const.bindings
  }
;;

let decode =
  let open Dune_lang.Decoder in
  fields
  @@ let+ flags = field Fields.flags ~default:default.flags Variable.Flag.Set.decode
     and+ sys = field Fields.sys ~default:default.sys Variable.Sys.Bindings.decode in
     let const = default.const in
     { flags; sys; const }
;;

let to_dyn { flags; sys; const } =
  Dyn.record
    [ Fields.flags, Variable.Flag.Set.to_dyn flags
    ; Fields.sys, Variable.Sys.Bindings.to_dyn sys
    ; Fields.const, Variable.Const.Bindings.to_dyn const
    ]
;;

let equal { flags; sys; const } t =
  Variable.Flag.Set.equal flags t.flags
  && Variable.Sys.Bindings.equal sys t.sys
  && Variable.Const.Bindings.equal const t.const
;;

let sys { sys; _ } = sys
let set_sys t sys = { t with sys }
let clear_flags t = { t with flags = Variable.Flag.Set.empty }

let pp { flags; sys; const } =
  let pp_section heading pp_section =
    (* The hbox is to prevent long values in [pp_section] from causing the heading to wrap. *)
    let pp_heading = Pp.hbox (Pp.text heading) in
    Pp.concat ~sep:Pp.newline [ pp_heading; pp_section ]
  in
  Pp.enumerate
    ~f:Fun.id
    [ pp_section "Flags" (Variable.Flag.Set.pp flags)
    ; pp_section "System Environment Variables" (Variable.Sys.Bindings.pp sys)
    ; pp_section "Constants" (Variable.Const.Bindings.pp const)
    ]
;;

module Variable_value = struct
  type t =
    | Bool of bool
    | String of string
    | Unset_sys
end

let get t variable =
  match (variable : Variable.t) with
  | Flag flag -> Variable_value.Bool (Variable.Flag.Set.mem t.flags flag)
  | Sys sys ->
    (match Variable.Sys.Bindings.get t.sys sys with
     | Some value -> String value
     | None -> Unset_sys)
  | Const const -> String (Variable.Const.Bindings.get t.const const)
;;
