open Import

module T = struct
  type t = OpamVariable.t

  let to_dyn s = Dyn.string (OpamVariable.to_string s)
  let compare x y = Ordering.of_int (OpamVariable.compare x y)
end

module Map = Map.Make (T)
module Set = Set.Make (T) (Map)
include T

let to_opam = Fun.id
let of_opam = Fun.id
let equal = OpamVariable.equal

include (
  Dune_util.Stringlike.Make (struct
    type t = OpamVariable.t

    let to_string x = OpamVariable.to_string x
    let module_ = "Variable_name"
    let description = "variable name"
    let description_of_valid_string = None
    let hint_valid = None
    let of_string_opt s = if s = "" then None else Some (OpamVariable.of_string s)
  end) :
    Dune_util.Stringlike with type t := t)

let hash t = String.hash (to_string t)
let digest_feed = Dune_digest.Feed.contramap Dune_digest.Feed.string ~f:to_string
let arch = of_string "arch"
let os = of_string "os"
let os_version = of_string "os-version"
let os_distribution = of_string "os-distribution"
let os_family = of_string "os-family"
let opam_version = of_string "opam-version"
let sys_ocaml_version = of_string "sys-ocaml-version"
let with_test = of_string "with-test"
let with_doc = of_string "with-doc"
let with_dev_setup = of_string "with-dev-setup"
let version = of_string "version"
let name = of_string "name"
let build = of_string "build"
let post = of_string "post"
let dev = of_string "dev"
let one_of t xs = List.mem xs ~equal t

let platform_specific =
  Set.of_list [ arch; os; os_version; os_distribution; os_family; sys_ocaml_version ]
;;

(* Users are free to refer to arbitrarily-named variables, but these are the
   common variables that are widely in circulation. *)
let all_known =
  [ arch
  ; os
  ; os_version
  ; os_distribution
  ; os_family
  ; opam_version
  ; sys_ocaml_version
  ; with_test
  ; with_doc
  ; with_dev_setup
  ; version
  ; name
  ; build
  ; post
  ; dev
  ]
;;

let encode t = Encoder.string (to_string t)

let check_typo_underscore_instead_of_dash =
  let possible_typos =
    List.filter_map all_known ~f:(fun t ->
      let t_string = to_string t in
      if String.contains t_string '-'
      then Some (String.replace_char t_string ~from:'-' ~to_:'_')
      else None)
  in
  fun loc t ->
    let string = to_string t in
    if List.mem possible_typos string ~equal:String.equal
    then
      List.iter all_known ~f:(fun v ->
        (* Many of dune's pform variables use underscores to separate words, but
           package variables use underscores for compatibility with opam. Thus we
           expect users to often mistakenly use underscores for package variables,
           so warn in this case. *)
        let v_string = to_string v in
        let possible_typo = String.replace_char v_string ~from:'-' ~to_:'_' in
        if String.equal string possible_typo
        then
          User_warning.emit
            ~loc
            [ Pp.textf
                "The package variable %S looks like a typo. Did you mean %S?"
                string
                v_string
            ])
;;

let decode =
  let open Decoder in
  let+ loc, t = located (string >>| of_string) in
  check_typo_underscore_instead_of_dash loc t;
  t
;;

module Project = struct
  let encode name = Encoder.string (":" ^ to_string name)

  let decode =
    Decoder.atom_matching ~desc:"variable" (fun s ->
      if String.is_prefix s ~prefix:":" then Some (of_string (String.drop s 1)) else None)
  ;;
end
