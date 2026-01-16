let all = ref []
let register ~name f = all := (name, f) :: !all

let dump () =
  ListLabels.map !all ~f:(fun (name, f) ->
    ( name
    , try f () with
      | exn -> Exn.to_dyn exn ))
;;
