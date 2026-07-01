module Build = struct
  let count = Counter.create ()
  let process_wall_clock = Counter.Timer.create ()
  let user = Counter.Timer.create ()
  let system = Counter.Timer.create ()

  let add_process_times ~elapsed_time ~user_cpu_time ~system_cpu_time =
    Counter.incr count;
    Counter.Timer.add process_wall_clock elapsed_time;
    (match user_cpu_time with
     | None -> ()
     | Some user_cpu_time -> Counter.Timer.add user user_cpu_time);
    match system_cpu_time with
    | None -> ()
    | Some system_cpu_time -> Counter.Timer.add system system_cpu_time
  ;;

  let process_count () = Counter.read count
  let process_time () = Counter.Timer.read process_wall_clock
  let process_user_cpu_time () = Counter.Timer.read user
  let process_system_cpu_time () = Counter.Timer.read system

  let reset () =
    Counter.reset count;
    Counter.Timer.reset process_wall_clock;
    Counter.Timer.reset user;
    Counter.Timer.reset system
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
