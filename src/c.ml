open Stdune

module Kind = struct
  type t =
    | C
    | Cxx

  let pp fmt t : unit =
    match t with
    | C -> Format.pp_print_string fmt "c"
    | Cxx -> Format.pp_print_string fmt "cpp"

  let split_extension fn =
    match String.lsplit2 fn ~on:'.' with
    | Some (obj, "c") -> Some (obj, C)
    | Some (obj, "cpp") -> Some (obj, Cxx)
    | _ -> None

  let possible_fns t fn =
    match t with
    | C -> [fn ^ ".c"]
    | Cxx -> [fn ^ ".cpp"]

  module Dict = struct
    type 'a t =
      { c : 'a
      ; cxx : 'a
      }

    let make a =
      { c = a
      ; cxx = a
      }

    let get { c; cxx } = function
      | C -> c
      | Cxx -> cxx

    let add t k v =
      match k with
      | C -> { t with c = v }
      | Cxx -> { t with cxx = v }

    let update t k ~f =
      let v = get t k in
      add t k (f v)

    let merge t1 t2 ~f =
      { c = f t1.c t2.c
      ; cxx = f t1.cxx t2.cxx
      }
  end
end

module Source = struct
  type t =
    { kind : Kind.t
    ; path : Path.t
    }

  let kind t = t.kind
  let path t = t.path
  let src_dir t = Path.parent_exn t.path

  let make ~kind ~path =
    { kind
    ; path
    }
end

module Sources = struct
  type t = (Loc.t * Source.t) String.Map.t

  let objects (t : t) ~dir ~ext_obj =
    String.Map.keys t
    |> List.map ~f:(fun c -> Path.relative dir (c ^ ext_obj))

  let split_by_kind t =
    let (c, cxx) =
      String.Map.partition t ~f:(fun (_, s) ->
        match (Source.kind s : Kind.t) with
        | C -> true
        | Cxx -> false)
    in
    {Kind.Dict. c; cxx}
end
