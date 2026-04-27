open Import

type t =
  { root : string
  ; where : Dune_rpc.Where.t
  ; registry : [ `Add | `Skip ]
  ; mutable path : string option
  }

let create ~root ~where registry = { root; where; registry; path = None }

let cleanup t =
  match t.path with
  | None -> ()
  | Some path ->
    Fpath.unlink_no_err path;
    t.path <- None
;;

let normalize_where = function
  | `Ip _ as where -> where
  | `Unix path ->
    `Unix
      (if Filename.is_relative path then Filename.concat (Sys.getcwd ()) path else path)
;;

let register t =
  match t.registry with
  | `Skip -> ()
  | `Add ->
    let (`Caller_should_write { Dune_rpc.Registry.File.path; contents }) =
      let registry_config = Dune_rpc.Registry.Config.create (Lazy.force Dune_util.xdg) in
      let dune =
        let pid = Unix.getpid () in
        let where = normalize_where t.where in
        Dune_rpc.Registry.Dune.create ~where ~root:t.root ~pid
      in
      Dune_rpc.Registry.Config.register registry_config dune
    in
    let (_ : Fpath.mkdir_p_result) = Fpath.mkdir_p (Filename.dirname path) in
    Io.String_path.write_file path contents;
    t.path <- Some path;
    at_exit (fun () -> cleanup t)
;;
