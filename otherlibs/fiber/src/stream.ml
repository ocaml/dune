open Core
open Core.O
open Stdune

module In = struct
  (* Invariant: once [read] has returned [None], it always returns [None] *)
  type nonrec 'a t =
    { mutable read : unit -> 'a option t
    ; mutable reading : bool
    }

  let create_unchecked read = { read; reading = false }

  let create read =
    let t = { read; reading = false } in
    let read () =
      let+ x = read () in
      if Option.is_none x then t.read <- (fun () -> return None);
      x
    in
    t.read <- read;
    t

  let lock t =
    if t.reading then Code_error.raise "Fiber.Stream.In: already reading" [];
    t.reading <- true

  let unlock t = t.reading <- false

  let read t =
    lock t;
    let+ x = t.read () in
    unlock t;
    x

  let empty () = create_unchecked (fun () -> return None)

  let concat (type a) (xs : a t list) =
    let remains = ref xs in
    let rec go () =
      match !remains with
      | [] -> return None
      | x :: xs -> (
        let* v = read x in
        match v with
        | Some v -> return (Some v)
        | None ->
          remains := xs;
          go ())
    in
    create go

  let append x y = concat [ x; y ]

  let of_list xs =
    let xs = ref xs in
    create_unchecked (fun () ->
        match !xs with
        | [] -> return None
        | x :: xs' ->
          xs := xs';
          return (Some x))

  let cons a t = concat [ of_list [ a ]; t ]

  let filter_map t ~f =
    let rec read () =
      t.read () >>= function
      | None ->
        unlock t;
        return None
      | Some x -> (
        match f x with
        | None -> read ()
        | Some y -> return (Some y))
    in
    lock t;
    create_unchecked read

  let sequential_iter t ~f =
    let rec loop t ~f =
      t.read () >>= function
      | None ->
        unlock t;
        return ()
      | Some x ->
        let* () = f x in
        loop t ~f
    in
    lock t;
    loop t ~f

  let parallel_iter t ~f k =
    let n = ref 1 in
    let k () =
      decr n;
      if !n = 0 then (
        unlock t;
        k ())
      else end_of_fiber
    in
    let rec loop t =
      t.read () (function
        | None -> k ()
        | Some x ->
          incr n;
          fork (fun () -> f x k) (fun () -> loop t))
    in
    lock t;
    loop t
end

module Out = struct
  type nonrec 'a t =
    { mutable write : 'a option -> unit t
    ; mutable writing : bool
    }

  let lock t =
    if t.writing then Code_error.raise "Fiber.Stream.Out: already writing" [];
    t.writing <- true

  let unlock t = t.writing <- false

  let create write =
    let t = { write; writing = false } in
    let write x =
      if Option.is_none x then
        t.write <-
          (function
          | None -> return ()
          | Some _ ->
            Code_error.raise "Fiber.Stream.Out: stream output closed" []);
      write x
    in
    t.write <- write;
    t

  let write t x =
    lock t;
    let+ () = t.write x in
    unlock t

  let null () = create (fun _ -> return ())
end

let connect i o =
  In.lock i;
  Out.lock o;
  let rec go () =
    let* a = i.read () in
    let* () = o.write a in
    match a with
    | None ->
      In.unlock i;
      Out.unlock o;
      return ()
    | Some _ -> go ()
  in
  go ()

let supply i o =
  In.lock i;
  Out.lock o;
  let rec go () =
    let* a = i.read () in
    match a with
    | None ->
      In.unlock i;
      Out.unlock o;
      return ()
    | Some _ ->
      let* () = o.write a in
      go ()
  in
  go ()

let pipe () =
  let mvar = Mvar.create () in
  let i = In.create (fun () -> Mvar.read mvar) in
  let o = Out.create (fun x -> Mvar.write mvar x) in
  (i, o)
