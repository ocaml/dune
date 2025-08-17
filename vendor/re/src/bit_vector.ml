type t =
  { len : int
  ; bits : Bytes.t
  }

let byte s i = Char.code (Bytes.unsafe_get s i)
let set_byte s i x = Bytes.unsafe_set s i (Char.chr x)
let length t = t.len

let unsafe_set v n b =
  let i = n lsr 3 in
  let c = byte v.bits i in
  let mask = 1 lsl (n land 7) in
  set_byte v.bits i (if b then c lor mask else c land lnot mask)
;;

let set v n b =
  if n < 0 || n >= v.len then invalid_arg "Bit_vector.set";
  unsafe_set v n b
;;

let unsafe_get v n =
  let i = n lsr 3 in
  byte v.bits i land (1 lsl (n land 7)) > 0
;;

let get v n =
  if n < 0 || n >= v.len then invalid_arg "Bit_vector.get";
  unsafe_get v n
;;

let reset_zero t = Bytes.fill t.bits 0 (Bytes.length t.bits) '\000'

let create_zero len =
  let bits =
    let r = len land 7 in
    let q = len lsr 3 in
    let len = if r = 0 then q else q + 1 in
    Bytes.make len '\000'
  in
  { len; bits }
;;

let pp fmt { len; bits } =
  let len fmt () = Fmt.sexp fmt "len" Fmt.int len in
  let bits fmt () = Fmt.sexp fmt "bits" Fmt.bytes bits in
  Format.fprintf fmt "%a@.%a@." len () bits ()
;;
