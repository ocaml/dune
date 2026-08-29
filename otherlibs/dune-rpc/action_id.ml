open Import

type t = string

let to_string t = t
let of_string s = s
let conv = Conv.string

module Table = String.Table

let gen =
  let prng = lazy (Random.State.make_self_init ()) in
  fun () ->
    let state = Lazy.force prng in
    Printf.sprintf
      "%08x%08x%08x%08x"
      (Random.State.bits state)
      (Random.State.bits state)
      (Random.State.bits state)
      (Random.State.bits state)
;;
