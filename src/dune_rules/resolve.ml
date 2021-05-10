open Stdune
open Dune_engine
include Or_exn

let of_result t = t

let read_memo_build = function
  | Ok x -> Memo.Build.return x
  | Error exn ->
    let open Memo.Build.O in
    let+ () = Memo.Build.return () in
    raise exn

let read = function
  | Ok x -> Action_builder.return x
  | Error exn -> Action_builder.delayed (fun () -> raise exn)

let args = function
  | Ok args -> args
  | Error exn -> Command.Args.Fail { fail = (fun () -> raise exn) }

let fail msg = Error (User_error.E (msg, None))

let peek t = Result.map_error t ~f:ignore

let is_ok t = Result.is_ok (peek t)

let is_error t = Result.is_error (peek t)

let extend_dep_path entry t =
  Result.map_error t ~f:(fun exn -> Dep_path.prepend_exn exn entry)

module Build = struct
  let bind t ~f =
    match t with
    | Error _ as err -> Memo.Build.return err
    | Ok x -> Memo.Build.map (f x) ~f:Fun.id
end

module List = struct
  let map = Result.List.map

  let filter_map = Result.List.filter_map

  let concat_map = Result.List.concat_map

  let iter = Result.List.iter

  let fold_left = Result.List.fold_left
end

let all = List.map ~f:Fun.id

module Option = struct
  let iter x ~f =
    match x with
    | None -> return ()
    | Some x -> f x
end
