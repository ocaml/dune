open Import

let term =
  let+ builder = Common.Builder.term in
  let common, config = Common.init builder in
  Scheduler.go ~common ~config
  @@ fun () ->
  let open Fiber.O in
  let* setup = Import.Main.setup () in
  let+ setup = Memo.run setup in
  let ctxts =
    List.map
      ~f:(fun (name, _) -> Context_name.to_string name)
      (Context_name.Map.to_list setup.scontexts)
  in
  List.iter ctxts ~f:print_endline
;;

let command =
  let doc = "List the build contexts available in the workspace." in
  let info = Cmd.info ~doc "contexts" in
  Cmd.v info term
;;
