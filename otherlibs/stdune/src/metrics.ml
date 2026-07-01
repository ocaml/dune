module Build = struct
  let count = Counter.create ()
  let process_time_counter = Counter.Timer.create ()

  let add_process_time process_time =
    Counter.incr count;
    Counter.Timer.add process_time_counter process_time
  ;;

  let process_time () =
    Option.some_if (Counter.read count > 0) (Counter.Timer.read process_time_counter)
  ;;

  let reset () =
    Counter.reset count;
    Counter.Timer.reset process_time_counter
  ;;
end

let reset () = ()

module type Stat = sig
  val count : Counter.t
  val bytes : Counter.t
  val time : Counter.Timer.t
end

module File_read = struct
  let count = Counter.create ()
  let bytes = Counter.create ()
  let time = Counter.Timer.create ()
end

module File_write = struct
  let count = Counter.create ()
  let bytes = Counter.create ()
  let time = Counter.Timer.create ()
end

module Directory_read = struct
  let count = Counter.create ()
  let time = Counter.Timer.create ()
end

module Digest = struct
  module File = struct
    let count = Counter.create ()
    let bytes = Counter.create ()
    let time = Counter.Timer.create ()
  end

  module Value = struct
    let count = Counter.create ()
    let bytes = Counter.create ()
    let time = Counter.Timer.create ()
  end
end
