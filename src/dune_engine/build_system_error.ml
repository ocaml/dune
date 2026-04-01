open Import
module Id = Id.Make ()

type t =
  | Exn of
      { id : Id.t
      ; exn : Exn_with_backtrace.t
      }
  | Diagnostic of
      { id : Id.t
      ; diagnostic : Compound_user_error.t
      ; dir : Path.t option
      ; promotion : User_message.Diff_annot.t option
      }

module Event = struct
  type nonrec t =
    | Add of t
    | Remove of t
end

let of_exn (exn : Exn_with_backtrace.t) =
  let exn =
    match exn.exn with
    | Memo.Error.E e -> { exn with exn = Memo.Error.get e }
    | _ -> exn
  in
  match exn.exn with
  | User_error.E main ->
    let dir = Option.map ~f:Path.of_string main.dir in
    let promotion = main.promotion in
    (match main.compound with
     | [] ->
       [ Diagnostic
           { dir
           ; id = Id.gen ()
           ; diagnostic = Compound_user_error.make ~main ~related:[]
           ; promotion
           }
       ]
     | diagnostics ->
       List.map diagnostics ~f:(fun diagnostic ->
         Diagnostic { id = Id.gen (); diagnostic; dir; promotion }))
  | _ -> [ Exn { id = Id.gen (); exn } ]
;;

let promotion = function
  | Exn _ -> None
  | Diagnostic d -> d.promotion
;;

let id = function
  | Exn d -> d.id
  | Diagnostic d -> d.id
;;

let dir = function
  | Exn _ -> None
  | Diagnostic d -> d.dir
;;

let description = function
  | Exn e -> `Exn e.exn
  | Diagnostic d -> `Diagnostic d.diagnostic
;;

module Set = struct
  type error = t
  type t = error Id.Map.t

  let add t error = Id.Map.set t (id error) error
  let equal a b = Id.Map.equal a b ~equal:(fun a b -> Id.equal (id a) (id b))
  let current t = t
  let empty = Id.Map.empty
end

module For_tests = struct
  let make ~description ~dir ~promotion () =
    let id = Id.gen () in
    match description with
    | `Exn exn -> Exn { id; exn }
    | `Diagnostic diagnostic -> Diagnostic { id; diagnostic; dir; promotion }
  ;;
end
