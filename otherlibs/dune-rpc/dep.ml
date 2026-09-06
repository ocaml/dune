open Import

module T = struct
  type t =
    | File of string
    | Directory of string
    | Glob of
        { path : string
        ; glob : string
        }

  let conv =
    let open Conv in
    let file = constr "File" string (fun s -> File s) in
    let directory = constr "Directory" string (fun s -> Directory s) in
    let glob_cstr =
      constr "Glob" (pair string string) (fun (path, glob) -> Glob { path; glob })
    in
    sum
      [ econstr file; econstr directory; econstr glob_cstr ]
      (function
        | File s -> case s file
        | Directory s -> case s directory
        | Glob { path; glob } -> case (path, glob) glob_cstr)
  ;;

  let repr =
    Repr.variant
      "dependency"
      [ Repr.case "File" Repr.string ~proj:(function
          | File path -> Some path
          | Directory _ | Glob _ -> None)
      ; Repr.case "Directory" Repr.string ~proj:(function
          | Directory path -> Some path
          | File _ | Glob _ -> None)
      ; Repr.case
          "Glob"
          Repr.(pair string string)
          ~proj:(function
            | Glob { path; glob } -> Some (path, glob)
            | File _ | Directory _ -> None)
      ]
  ;;

  include Repr.Poly (struct
      type nonrec t = t

      let repr = repr
    end)

  let to_dyn = Repr.to_dyn repr
end

include T
module O = Comparable.Make (T)

module Set = struct
  include O.Set

  let conv : t Conv.value = Conv.iso (Conv.list conv) of_list to_list
end
