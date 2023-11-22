open Import

module Variable = struct
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
      let open Decoder in
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
        let open Decoder in
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

      let extend t t' = Map.superpose t' t

      let pp t =
        Pp.enumerate all ~f:(fun variable ->
          match Map.find t variable with
          | Some value ->
            Pp.textf "%s = %s" (to_string variable) (String.maybe_quoted value)
          | None -> Pp.textf "%s (unset)" (to_string variable))
      ;;
    end
  end

  module User = struct
    type t = Package_variable.Name.t

    module Map = Package_variable.Name.Map

    module Bindings = struct
      type t = string Map.t

      let empty = Map.empty
      let to_dyn = Map.to_dyn Dyn.string
      let equal = Map.equal ~equal:String.equal
      let get = Map.find
      let set = Map.set

      let decode =
        let open Decoder in
        let+ loc, bindings =
          located (repeat (pair Package_variable.Name.decode string))
        in
        match Map.of_list bindings with
        | Ok t -> t
        | Error (duplicate_key, a, b) ->
          User_error.raise
            ~loc
            [ Pp.textf
                "Duplicate entries for user variable %s (%s, %s)"
                (String.maybe_quoted (Package_variable.Name.to_string duplicate_key))
                (String.maybe_quoted a)
                (String.maybe_quoted b)
            ]
      ;;

      let pp t =
        if Map.is_empty t
        then Pp.text "(none)"
        else
          Pp.enumerate (Map.to_list t) ~f:(fun (variable, value) ->
            Pp.textf
              "%s = %s"
              (Package_variable.Name.to_string variable)
              (String.maybe_quoted value))
      ;;
    end
  end

  module Const = struct
    type t = [ `Opam_version ]

    let to_string = function
      | `Opam_version -> "opam-version"
    ;;

    let to_dyn t = Dyn.string (to_string t)
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

  module T = struct
    type t =
      | Sys of Sys.t
      | User of User.t
      | Const of Const.t

    let to_dyn = function
      | Sys sys -> Dyn.variant "Sys" [ Sys.to_dyn sys ]
      | User user -> Dyn.variant "User" [ Package_variable.Name.to_dyn user ]
      | Const const -> Dyn.variant "Const" [ Const.to_dyn const ]
    ;;

    let of_string_opt string =
      match Sys.of_string_opt string with
      | Some sys -> Some (Sys sys)
      | None ->
        (match Const.of_string_opt string with
         | Some const -> Some (Const const)
         | None -> Some (User (Package_variable.Name.of_string string)))
    ;;

    let to_string = function
      | Sys sys -> Sys.to_string sys
      | User user -> Package_variable.Name.to_string user
      | Const const -> Const.to_string const
    ;;

    let compare a b = String.compare (to_string a) (to_string b)
    let equal a b = String.equal (to_string a) (to_string b)

    let decode =
      let open Decoder in
      let+ loc, string = located string in
      match of_string_opt string with
      | Some t -> t
      | None ->
        User_error.raise
          ~loc
          [ Pp.textf "No such variable: %s" (String.maybe_quoted string) ]
    ;;

    let encode t = Encoder.string (to_string t)
  end

  include Comparable.Make (T)
  include T
end

type t =
  { sys : Variable.Sys.Bindings.t
  ; user : Variable.User.Bindings.t
  ; const : Variable.Const.Bindings.t
  }

module Fields = struct
  let sys = "sys"
  let user = "user"
  let const = "const"
end

let create ~sys ~user = { sys; user; const = Variable.Const.bindings }
let default = create ~sys:Variable.Sys.Bindings.empty ~user:Variable.User.Map.empty

let decode =
  let open Decoder in
  fields
  @@
  let+ sys = field Fields.sys ~default:default.sys Variable.Sys.Bindings.decode
  and+ user = field Fields.user ~default:default.user Variable.User.Bindings.decode in
  let const = default.const in
  { sys; user; const }
;;

let to_dyn { sys; const; user } =
  Dyn.record
    [ Fields.sys, Variable.Sys.Bindings.to_dyn sys
    ; Fields.user, Variable.User.Bindings.to_dyn user
    ; Fields.const, Variable.Const.Bindings.to_dyn const
    ]
;;

let equal { sys; user; const } t =
  Variable.Sys.Bindings.equal sys t.sys
  && Variable.User.Bindings.equal user t.user
  && Variable.Const.Bindings.equal const t.const
;;

let sys { sys; _ } = sys
let set_sys t sys = { t with sys }

let pp =
  let pp_section heading pp_section =
    (* The hbox is to prevent long values in [pp_section] from causing the heading to wrap. *)
    let pp_heading = Pp.hbox (Pp.text heading) in
    Pp.concat ~sep:Pp.space [ pp_heading; pp_section ] |> Pp.vbox
  in
  fun { sys; user; const } ->
    Pp.enumerate
      ~f:Fun.id
      [ pp_section "System Environment Variables" (Variable.Sys.Bindings.pp sys)
      ; pp_section "User" (Variable.User.Bindings.pp user)
      ; pp_section "Constants" (Variable.Const.Bindings.pp const)
      ]
;;

module Variable_value = struct
  type t =
    | String of string
    | Unset
end

let get t variable =
  match (variable : Variable.t) with
  | Const const -> Variable_value.String (Variable.Const.Bindings.get t.const const)
  | User user ->
    (match Variable.User.Bindings.get t.user user with
     | Some value -> String value
     | None -> Unset)
  | Sys sys ->
    (match Variable.Sys.Bindings.get t.sys sys with
     | Some value -> String value
     | None -> Unset)
;;
