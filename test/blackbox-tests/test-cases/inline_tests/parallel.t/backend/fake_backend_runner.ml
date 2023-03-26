module StringSet = Set.Make(String)

let usage_msg = ""

let list_partitions = ref false

let partition = ref ""

let libname = ref ""

let anon_fun _ = ()

let speclist =
  [ ("--libname", Arg.Set_string libname, "libname")
  ; ("--partition", Arg.Set_string partition, "partition")
  ; ("--list-partitions", Arg.Set list_partitions, "list partitions")
  ]

type t =
  { libname : string
  ; partition : string
  ; name : string
  ; run : unit -> unit
  }

let tests = ref []

let register ~libname ~partition name run =
  tests := { libname; partition; name; run } :: !tests

let run () =
  Arg.parse speclist anon_fun usage_msg;
  if !list_partitions then
    let partitions =
      List.fold_left
        (fun acc t -> StringSet.add t.partition acc)
        StringSet.empty !tests
    in
    StringSet.iter print_endline partitions
  else if !libname = "" then failwith "Should specify libname";
  List.iter
    (fun t ->
      if t.libname = !libname && t.partition = !partition then (
        Printf.printf "Running %s\n" t.name;
        try
          t.run ();
          Printf.printf "Ok\n"
        with e -> Printf.printf "Error: %s\n" (Printexc.to_string e)))
    (List.rev !tests)
