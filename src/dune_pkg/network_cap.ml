type t = { reason_for_network_access : string }

let create ~reason_for_network_access = { reason_for_network_access }

let log { reason_for_network_access } =
  match !Dune_engine.Clflags.display with
  | Quiet | Short -> ()
  | Verbose ->
    Dune_console.printf "Performing network access because: %s" reason_for_network_access
;;

let for_unit_test = create ~reason_for_network_access:"unit test"
