open Import

type t =
  { root : string option
  ; switch : string
  }

let to_dyn { root; switch } =
  Dyn.(record [ "root", option string root; "switch", string switch ])
;;

let equal { root; switch } t =
  Option.equal String.equal root t.root && String.equal switch t.switch
;;

let hash { root; switch } =
  Tuple.T2.hash (Option.hash String.hash) String.hash (root, switch)
;;

let opam_switch_prefix_var_name = "OPAM_SWITCH_PREFIX"
