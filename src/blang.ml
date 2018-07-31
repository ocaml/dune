open! Stdune

module Op = struct
  type t =
    | Eq
    | Gt
    | Gte
    | Lte
    | Lt
    | Neq

  let eval t (x : Ordering.t) =
    match t, x with
    | (Eq  | Gte | Lte) , Eq
    | (Neq | Lt  | Lte) , Lt
    | (Neq | Gt  | Gte) , Gt -> true
    | _, _ -> false
end

type 'a t =
  | Expr of 'a
  | And of 'a t list
  | Or of 'a t list
  | Compare of Op.t * 'a * 'a

type 'a expander =
  { f : 'value. mode:'value String_with_vars.Mode.t
      -> 'a
      -> Loc.t * 'value
  }

let rec eval_bool t ~dir ~(f : 'a expander) =
  match t with
  | Expr a ->
    begin match f.f ~mode:Single a with
    | _, String "true" -> true
    | _, String "false" -> false
    | loc, _ -> Loc.fail loc "This value must be either true or false"
    end
  | And xs -> List.for_all ~f:(eval_bool ~f ~dir) xs
  | Or xs -> List.exists ~f:(eval_bool ~f ~dir) xs
  | Compare (op, x, y) ->
    let ((_, x), (_, y)) = (f.f ~mode:Many x, f.f ~mode:Many y) in
    Value.L.compare_vals ~dir x y
    |> Op.eval op
