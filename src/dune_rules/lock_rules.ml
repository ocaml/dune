open Import

module Spec = struct
  type ('path, 'target) t = {
    target: 'target;
    lock_dir : string;
  }

  let name = "lock"
  let version = 1
  let bimap t _ g = { t with target = g t.target }
  let is_useful_to ~memoize = memoize

  let encode {target; lock_dir} _encode_path encode_target : Sexp.t =
    Sexp.List [
      encode_target target;
      Sexp.Atom lock_dir
    ]

  let action {target=_; lock_dir=_} ~ectx:_ ~eenv:_ =
    failwith "later"
end

module A = Action_ext.Make (Spec)

let lock () =
  failwith "TODO"
