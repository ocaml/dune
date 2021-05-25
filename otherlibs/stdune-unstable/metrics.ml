let enabled = ref false

let enable () = enabled := true

module Reset = Monoid.Endofunction.Left (Unit)

let reset = ref Reset.empty

module Timer = struct
  type t = float ref

  let create () =
    let timer = ref 0. in
    reset := Reset.combine !reset (fun () -> timer := 0.);
    timer

  let read_seconds t = !t

  let record t ~f =
    match !enabled with
    | false -> f ()
    | true ->
      let start = Unix.gettimeofday () in
      Exn.protect ~f ~finally:(fun () ->
          t := !t +. (Unix.gettimeofday () -. start))
end

let reset () = !reset ()
