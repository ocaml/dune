open Import

type 'a t = { mutable state : 'a state }

and 'a state =
  | Return of 'a
  | Sleep of 'a handlers
  | Repr of 'a t

and 'a handlers =
  | Empty
  | One    of ('a -> unit)
  | Append of 'a handlers * 'a handlers

let append h1 h2 =
  match h1, h2 with
  | Empty, _ -> h2
  | _, Empty -> h1
  | _ -> Append (h1, h2)

let rec repr t =
  match t.state with
  | Repr t' -> let t'' = repr t' in if t'' != t' then t.state <- Repr t''; t''
  | _       -> t

let run_handlers handlers x =
  let rec loop handlers acc =
    match handlers, acc with
    | Empty, [] -> ()
    | Empty, h :: acc -> loop h acc
    | One f, [] -> f x
    | One f, h :: acc -> f x; loop h acc
    | Append (h1, h2), _ -> loop h1 (h2 :: acc)
  in
  loop handlers []


let connect t1 t2 =
  let t1 = repr t1 and t2 = repr t2 in
  match t1.state with
  | Sleep h1 ->
    if t1 == t2 then
      ()
    else begin
      match t2.state with
      | Repr _ -> assert false
      | Sleep h2 ->
        t2.state <- Repr t1;
        t1.state <- Sleep (append h1 h2)
      | Return x as state2 ->
        t1.state <- state2;
        run_handlers h1 x
    end
  | _ ->
    assert false

let return x = { state = Return x }

let sleeping () = { state = Sleep Empty }

let ( >>= ) t f =
  let t = repr t in
  match t.state with
  | Return v -> f v
  | Sleep handlers ->
    let res = sleeping () in
    t.state <- Sleep (append handlers (One (fun x -> connect res (f x))));
    res
  | Repr _ ->
    assert false

let create f =
  let t = sleeping () in
  f t;
  t

module Ivar = struct
  type nonrec 'a t = 'a t

  let fill t x =
    match t.state with
    | Repr _ -> assert false
    | Return _ -> failwith "Future.Ivar.fill"
    | Sleep handlers ->
      t.state <- Return x;
      run_handlers handlers x
end

let rec all = function
  | [] -> return []
  | x :: l ->
    x >>= fun x ->
    all l >>= fun l ->
    return (x :: l)

let rec all_unit = function
  | [] -> return ()
  | x :: l ->
    x >>= fun () ->
    all_unit l

type job =
  { prog : string
  ; args : string list
  ; stdout_to : string option
  ; ivar : unit Ivar.t
  }

let to_run : job Queue.t = Queue.create ()

let run ?stdout_to prog args =
  create (fun ivar ->
    Queue.push { prog; args; stdout_to; ivar } to_run)

module Scheduler = struct
  let command_line { prog; args; stdout_to; _ } =
    let s = String.concat (prog :: args) ~sep:" " in
    match stdout_to with
    | None -> s
    | Some fn -> sprintf "%s > %s" s fn

  let process_done job status =
    match status with
    | Unix.WEXITED 0 -> Ivar.fill job.ivar ()
    | _ ->
      Printf.ksprintf failwith "Process \"%s\" exited with status %d"
        (command_line job)
        (match status with
         | WEXITED n -> n
         | WSIGNALED n -> 128 + n
         | WSTOPPED _ -> assert false)

  let running = Hashtbl.create 128

  let rec wait_win32 () =
    let finished =
      Hashtbl.fold running ~init:[] ~f:(fun ~key:pid ~data:job acc ->
        let pid, status = Unix.waitpid [WNOHANG] pid in
        if pid <> 0 then begin
          process_done job status;
          pid :: acc
        end else
          acc)
    in
    match finished with
    | [] ->
      Unix.sleepf 0.001;
      wait_win32 ()
    | _ ->
      List.iter finished ~f:(Hashtbl.remove running)

  let rec go t =
    match (repr t).state with
    | Return v -> v
    | _ ->
      while Hashtbl.length running < !Clflags.concurrency && not (Queue.is_empty to_run) do
        let job = Queue.pop to_run in
        let stdout, close_stdout =
          match job.stdout_to with
          | None -> (Unix.stdout, false)
          | Some fn ->
            let fd = Unix.openfile fn [O_WRONLY; O_CREAT; O_TRUNC] 0o666 in
            (fd, true)
        in
        let pid =
          Unix.create_process job.prog (Array.of_list (job.prog :: job.args))
            Unix.stdin stdout Unix.stderr
        in
        if close_stdout then Unix.close stdout;
        Hashtbl.add running ~key:pid ~data:job
      done;
      if Sys.win32 then
        wait_win32 ()
      else begin
        let pid, status = Unix.wait () in
        process_done (Hashtbl.find running pid) status;
        Hashtbl.remove running pid
      end;
      go t
end
