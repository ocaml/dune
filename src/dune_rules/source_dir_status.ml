type t =
  | Data_only
  | Normal
  | Vendored

module Map = struct
  type 'a t =
    { data_only : 'a
    ; vendored : 'a
    ; normal : 'a
    }

  let equal f { data_only; vendored; normal } t =
    f data_only t.data_only && f vendored t.vendored && f normal t.normal
  ;;

  let merge x y ~f =
    { data_only = f x.data_only y.data_only
    ; vendored = f x.vendored y.vendored
    ; normal = f x.normal y.normal
    }
  ;;

  let find { data_only; vendored; normal } = function
    | Data_only -> data_only
    | Vendored -> vendored
    | Normal -> normal
  ;;

  let to_dyn f { data_only; vendored; normal } =
    let open Dyn in
    record [ "data_only", f data_only; "vendored", f vendored; "normal", f normal ]
  ;;

  let init ~f = { data_only = f Data_only; vendored = f Vendored; normal = f Normal }
end

let to_dyn t =
  let open Dyn in
  match t with
  | Data_only -> Variant ("Data_only", [])
  | Vendored -> Variant ("Vendored", [])
  | Normal -> Variant ("Normal", [])
;;

let to_string = function
  | Data_only -> "data_only"
  | Vendored -> "vendored"
  | Normal -> "normal"
;;

module Or_ignored = struct
  type nonrec t =
    | Ignored
    | Status of t
end

module Set = struct
  open Map

  type t = bool Map.t

  let all = { data_only = true; vendored = true; normal = true }
  let normal_only = { data_only = false; vendored = false; normal = true }

  let to_list { data_only; vendored; normal } =
    let acc = [] in
    let acc = if vendored then Vendored :: acc else acc in
    let acc = if data_only then Data_only :: acc else acc in
    let acc = if normal then Normal :: acc else acc in
    acc
  ;;
end
