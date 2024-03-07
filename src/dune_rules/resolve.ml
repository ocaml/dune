open Import

type error =
  { message : User_message.t
  ; stack_frames : User_message.Style.t Pp.t Lazy.t list
  }

type 'a t = ('a, error) result

let return x = Ok x
let bind = Result.bind

include Monad.Make (struct
    type nonrec 'a t = 'a t

    let return = return
    let bind = bind
  end)

let error_equal { message; stack_frames } b =
  User_message.equal message b.message
  && Stdune.List.equal
       (fun (lazy a) (lazy b) -> Poly.equal a b)
       stack_frames
       b.stack_frames
;;

let equal f = Result.equal f error_equal

let error_hash { message; stack_frames } =
  Poly.hash (Poly.hash message, Stdune.List.map stack_frames ~f:Lazy.force)
;;

let to_dyn f t =
  Result.to_dyn
    f
    Dyn.string
    (Result.map_error t ~f:(fun x -> User_message.to_string x.message))
;;

let hash f = Result.hash f error_hash
let of_result = Result.map_error ~f:(fun message -> { message; stack_frames = [] })
let to_result x = x

let push_frames { stack_frames; message = _ } f =
  let rec loop = function
    | [] ->
      let open Memo.O in
      let* () = Memo.return () in
      f ()
    | x :: rest ->
      Memo.push_stack_frame
        ~human_readable_description:(fun () -> Lazy.force x)
        (fun () -> loop rest)
  in
  loop stack_frames
;;

let error_to_memo error = push_frames error (fun () -> raise (User_error.E error.message))

let raise_error_with_stack_trace { message; stack_frames } =
  match
    Dune_util.Report_error.format_memo_stack (List.map stack_frames ~f:Lazy.force)
  with
  | None -> raise (User_error.E message)
  | Some stack ->
    let message = { message with paragraphs = message.paragraphs @ [ stack ] } in
    raise (User_error.E message)
;;

let read_memo = function
  | Ok x -> Memo.return x
  | Error err -> error_to_memo err
;;

let read = function
  | Ok x -> Action_builder.return x
  | Error err -> Action_builder.of_memo (error_to_memo err)
;;

let args t =
  match t with
  | Ok args -> args
  | Error _ ->
    let open Action_builder.O in
    Command.Args.Dyn (read t >>| fun _ -> assert false)
;;

let fail msg = Error { message = msg; stack_frames = [] }
let peek t = Result.map_error t ~f:ignore
let is_ok t = Result.is_ok (peek t)
let is_error t = Result.is_error (peek t)

let push_stack_frame ~human_readable_description:f t =
  match t with
  | Ok _ -> t
  | Error err -> Error { err with stack_frames = Lazy.from_fun f :: err.stack_frames }
;;

module List = struct
  let all = Result.List.all
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
  ;;
end

module Memo = struct
  open Memo.O

  module T = struct
    type nonrec 'a t = 'a t Memo.t

    let return x = Memo.return (Ok x)

    let bind t ~f =
      let* t = t in
      match t with
      | Ok s -> f s
      | Error e -> Memo.return (Error e)
    ;;
  end

  module M = struct
    include T
    include Monad.Make (T)
  end

  module List = Monad.List (M)
  include M

  let push_stack_frame ~human_readable_description f =
    let+ t = Memo.push_stack_frame ~human_readable_description f in
    push_stack_frame ~human_readable_description t
  ;;

  let lift t = Memo.return t
  let lift_memo t = Memo.map t ~f:(fun x -> Ok x)
  let is_ok t = Memo.map ~f:is_ok t
  let is_error t = Memo.map ~f:is_error t

  module Option = struct
    let iter t ~f : unit t =
      match t with
      | None -> return ()
      | Some t -> f t
    ;;
  end

  let all = List.map ~f:Fun.id

  let read_memo t =
    let* t = t in
    read_memo t
  ;;

  let read (type a) (t : a t) : a Action_builder.t = Action_builder.of_memo (read_memo t)
  let fail s = Memo.return (fail s)
  let args s = Command.Args.Dyn (read s)
  let of_result s = Memo.return (of_result s)
  let peek t = Memo.map t ~f:(Result.map_error ~f:ignore)
end
