open Import

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

  let map { data_only; vendored; normal } ~f =
    { data_only = f data_only; vendored = f vendored; normal = f normal }
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

module Per_dir = struct
  type nonrec t = t Filename.Map.t

  let status t ~dir =
    match Filename.Map.find t dir with
    | None -> Or_ignored.Ignored
    | Some s -> Or_ignored.Status s
  ;;
end

module Spec = struct
  type status = t
  type t = Predicate_lang.Glob.t Map.t
  type input = (Loc.t * Predicate_lang.Glob.t) option Map.t

  let default : t =
    let standard_dirs = Predicate_lang.Glob.of_glob (Glob.of_string "[!._]*") in
    { Map.normal = standard_dirs
    ; data_only = Predicate_lang.false_
    ; vendored = Predicate_lang.false_
    }
  ;;

  let create (t : input) =
    Map.init ~f:(fun kind ->
      match Map.find t kind with
      | None -> Map.find default kind
      | Some (_loc, s) -> s)
  ;;

  let eval (t : t) ~dirs : Per_dir.t =
    (* This function defines the unexpected behavior of: (dirs foo)
       (data_only_dirs bar)

       In this setup, bar is actually ignored rather than being data only. Because
       it was excluded from the total set of directories. *)
    let f =
      Map.merge t default ~f:(fun pred standard ->
        Predicate_lang.Glob.test pred ~standard)
    in
    Filename.Set.of_list dirs
    |> Filename.Set.to_map ~f:(fun _ -> ())
    |> Filename.Map.filter_mapi ~f:(fun dir () : status option ->
      match Map.map f ~f:(fun pred -> pred dir) |> Set.to_list with
      | [] -> None
      | statuses ->
        (* If a directory has a status other than [Normal], then the [Normal]
           status is irrelevant so we just filter it out. *)
        (match
           List.filter statuses ~f:(function
             | Normal -> false
             | _ -> true)
         with
         | [] -> Some Normal
         | [ status ] -> Some status
         | statuses ->
           (* CR-rgrinberg: this error needs a location *)
           User_error.raise
             [ Pp.textf
                 "Directory %s was marked as %s, it can't be marked as %s."
                 dir
                 (String.enumerate_and (List.map statuses ~f:to_string))
                 (match List.length statuses with
                  | 2 -> "both"
                  | _ -> "all these")
             ]))
  ;;
end
