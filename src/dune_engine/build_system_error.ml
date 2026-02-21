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
      ; promotion : Diff_promotion.Annot.t option
      }

module Event = struct
  type nonrec t =
    | Add of t
    | Remove of t
end

let severity_to_rpc = function
  | User_error.Severity.Error -> Dune_rpc_private.Diagnostic.Error
  | User_error.Severity.Warning -> Dune_rpc_private.Diagnostic.Warning
;;

let of_exn (exn : Exn_with_backtrace.t) =
  let exn =
    match exn.exn with
    | Memo.Error.E e -> { exn with exn = Memo.Error.get e }
    | _ -> exn
  in
  match exn.exn with
  | User_error.E main ->
    let related_diagnostics = User_error.related main in
    let dir = Option.map ~f:Path.of_string main.dir in
    let promotion = User_message.Annots.find main.annots Diff_promotion.Annot.annot in
    (match related_diagnostics with
     | _ :: _ ->
       List.map
         related_diagnostics
         ~f:
           (fun
             ({ User_error.Diagnostic.main
              ; User_error.Diagnostic.related
              ; User_error.Diagnostic.severity
              } :
               _)
           ->
           let related_messages =
             List.map
               related
               ~f:(fun ({ User_error.Related.loc; User_error.Related.message } : _) ->
                 User_message.make ~loc [ message ])
           in
           Diagnostic
             { dir
             ; id = Id.gen ()
             ; diagnostic =
                 Compound_user_error.make_with_severity
                   ~main
                   ~related:related_messages
                   ~severity:(severity_to_rpc severity)
             ; promotion
             })
     | [] ->
       [ Diagnostic
           { dir
           ; id = Id.gen ()
           ; diagnostic = Compound_user_error.make ~main ~related:[]
           ; promotion
           }
       ])
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

  type t =
    { current : error Id.Map.t
    ; stamp : int
    ; last_event : Event.t option
    }

  let add t error =
    let current = Id.Map.set t.current (id error) error in
    { current; stamp = t.stamp + 1; last_event = Some (Add error) }
  ;;

  let equal t { current; stamp; last_event } =
    Int.equal t.stamp stamp
    &&
    match t.last_event, last_event with
    | None, None ->
      assert (Id.Map.is_empty t.current && Id.Map.is_empty current);
      true (* only possible when both sets are empty *)
    | Some x, Some y ->
      (match x, y with
       | Add x, Add y -> Id.equal (id x) (id y)
       | Add _, _ -> false
       | Remove x, Remove y -> Id.equal (id x) (id y)
       | Remove _, _ -> false)
    | Some _, None | None, Some _ -> false
  ;;

  let one_event_diff ~prev ~next =
    if prev.stamp + 1 = next.stamp then next.last_event else None
  ;;

  let current t = t.current
  let empty = { current = Id.Map.empty; stamp = 0; last_event = None }
end

module For_tests = struct
  let make ~description ~dir ~promotion () =
    let id = Id.gen () in
    match description with
    | `Exn exn -> Exn { id; exn }
    | `Diagnostic diagnostic -> Diagnostic { id; diagnostic; dir; promotion }
  ;;
end
