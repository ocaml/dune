open Stdune
module List = ListLabels

external ev_version : unit -> int * int = "lev_version"

module Set (Element : sig
    type t

    val to_int : t -> int
  end) =
struct
  type t = int

  let equal = Int.equal
  let mem t c = t land Element.to_int c <> 0
  let singleton x : t = Element.to_int x
  let empty : t = 0
  let union x y = x lor y
  let add x y = union x (singleton y)
  let negate x = lnot x
  let inter x y = x land y
end

module Backend = struct
  type t =
    | Select
    | Poll
    | Epoll
    | Kqueue
    | Devpoll
    | Port
    | Linuxaio
    | Iouring

  let all = [ Select; Poll; Epoll; Kqueue; Devpoll; Port; Linuxaio; Iouring ]

  external select : unit -> int = "lev_backend_select"

  let select = select ()

  external poll : unit -> int = "lev_backend_poll"

  let poll = poll ()

  external epoll : unit -> int = "lev_backend_epoll"

  let epoll = epoll ()

  external kqueue : unit -> int = "lev_backend_kqueue"

  let kqueue = kqueue ()

  external devpoll : unit -> int = "lev_backend_devpoll"

  let devpoll = devpoll ()

  external port : unit -> int = "lev_backend_port"

  let port = port ()

  external linuxaio : unit -> int = "lev_backend_linuxaio"

  let linuxaio = linuxaio ()

  external iouring : unit -> int = "lev_backend_iouring"

  let iouring = iouring ()

  let to_int = function
    | Select -> select
    | Poll -> poll
    | Epoll -> epoll
    | Kqueue -> kqueue
    | Devpoll -> devpoll
    | Port -> port
    | Linuxaio -> linuxaio
    | Iouring -> iouring
  ;;

  module Set = struct
    include Set (struct
        type nonrec t = t

        let to_int = to_int
      end)

    let all = List.fold_left ~f:(fun x y -> to_int y lor x) all ~init:0
  end

  external supported : unit -> Set.t = "lev_backend_supported"
  external embeddable : unit -> Set.t = "lev_backend_embeddable"
  external recommended : unit -> Set.t = "lev_backend_recommended"
end

external sleep : Time.Span.t -> unit = "lev_sleep"

module Loop = struct
  module Flag = struct
    type t =
      | Backend of Backend.t
      | Auto
      | Noenv
      | Forkcheck
      | Noinotify
      | Signalfd
      | Nosigmask
      | Notimerfd

    external auto : unit -> int = "lev_loop_flags_auto"

    let auto = auto ()

    external noenv : unit -> int = "lev_loop_flags_noenv"

    let noenv = noenv ()

    external forkcheck : unit -> int = "lev_loop_flags_forkcheck"

    let forkcheck = forkcheck ()

    external noinotify : unit -> int = "lev_loop_flags_noinotify"

    let noinotify = noinotify ()

    external signalfd : unit -> int = "lev_loop_flags_signalfd"

    let signalfd = signalfd ()

    external nosigmask : unit -> int = "lev_loop_flags_nosigmask"

    let nosigmask = nosigmask ()

    external notimerfd : unit -> int = "lev_loop_flags_notimerfd"

    let notimerfd = notimerfd ()

    let to_int = function
      | Backend b -> Backend.to_int b
      | Auto -> auto
      | Noenv -> noenv
      | Forkcheck -> forkcheck
      | Noinotify -> noinotify
      | Signalfd -> signalfd
      | Nosigmask -> nosigmask
      | Notimerfd -> notimerfd
    ;;

    module Set = struct
      include Set (struct
          type nonrec t = t

          let to_int = to_int
        end)

      let of_backend_set x = x
    end
  end

  type t

  let flags = Flag.Set.singleton Auto

  external default : int -> t = "lev_ev_default"

  let default ?(flags = flags) () = default flags

  external create : int -> t = "lev_ev_create"

  let create ?(flags = flags) () = create flags

  external now : t -> Time.t = "lev_ev_now"
  external destroy : t -> unit = "lev_loop_destroy"
  external now_update : t -> unit = "lev_loop_now_update"
  external run : t -> int -> bool = "lev_ev_run"
  external is_default : t -> bool = "lev_loop_is_default"

  type run =
    | Once
    | Nowait

  external once : unit -> int = "lev_loop_run_once"

  let once = once ()

  external nowait : unit -> int = "lev_loop_run_nowait"

  let nowait = nowait ()

  let int_of_run = function
    | Once -> once
    | Nowait -> nowait
  ;;

  let run t v = if run t (int_of_run v) then `Otherwise else `No_more_active_watchers

  let rec run_until_done t =
    match run t Once with
    | `Otherwise -> run_until_done t
    | `No_more_active_watchers -> ()
  ;;

  external depth : t -> int = "lev_loop_depth"

  type break =
    | One
    | All
    | Cancel

  external one : unit -> int = "lev_loop_break_one_code"

  let one = one ()

  external all : unit -> int = "lev_loop_break_all_code"

  let all = all ()

  external cancel : unit -> int = "lev_loop_break_cancel_code"

  let cancel = cancel ()

  let int_of_break = function
    | One -> one
    | All -> all
    | Cancel -> cancel
  ;;

  external break : t -> int -> unit = "lev_loop_break"

  let break t b = break t (int_of_break b)

  external backend : t -> Backend.Set.t = "lev_loop_backend"

  let backend t =
    let b = backend t in
    List.find Backend.all ~f:(fun backend -> Backend.Set.mem b backend)
  ;;

  external suspend : t -> unit = "lev_loop_suspend"
  external resume : t -> unit = "lev_loop_resume"
  external ref : t -> unit = "lev_loop_ref"
  external unref : t -> unit = "lev_loop_unref"
  external feed_signal : signal:int -> unit = "lev_feed_signal"
  external feed_signal_event : t -> signal:int -> unit = "lev_loop_feed_signal_event"
end

module type Watcher = sig
  type t

  val start : t -> Loop.t -> unit
  val is_active : t -> bool
  val is_pending : t -> bool
  val stop : t -> Loop.t -> unit
  val destroy : t -> unit
end

module Watcher (S : sig
    type t
  end) =
struct
  open S

  external is_active : t -> bool = "lev_watcher_is_active"
  external is_pending : t -> bool = "lev_watcher_is_pending"
  external destroy : t -> unit = "lev_watcher_destroy"
end

module Io = struct
  module Event = struct
    type t =
      | Read
      | Write

    external read : unit -> int = "lev_io_read_code"

    let read = read ()

    external write : unit -> int = "lev_io_write_code"

    let write = write ()

    let to_int = function
      | Read -> read
      | Write -> write
    ;;

    module Set = struct
      include Set (struct
          type nonrec t = t

          let to_int = to_int
        end)

      let create ?(read = false) ?(write = false) () =
        union
          (if read then singleton Read else empty)
          (if write then singleton Write else empty)
      ;;
    end
  end

  type t

  include Watcher (struct
      type nonrec t = t
    end)

  external destroy : t -> unit = "lev_io_destroy"
  external fd : t -> Unix.file_descr = "lev_io_fd"
  external modify : t -> Event.Set.t -> unit = "lev_io_modify"

  external create
    :  (t -> Unix.file_descr -> Event.Set.t -> unit)
    -> Unix.file_descr
    -> Event.Set.t
    -> t
    = "lev_io_create"

  external start : t -> Loop.t -> unit = "lev_io_start"
  external stop : t -> Loop.t -> unit = "lev_io_stop"
end

let wrap_callback f t () = f t

module Periodic = struct
  type t

  include Watcher (struct
      type nonrec t = t
    end)

  external destroy : t -> unit = "lev_periodic_destroy"

  type kind =
    | Regular of
        { offset : Time.t
        ; interval : Time.Span.t option
        }
    | Custom of (t -> now:Time.t -> Time.t)

  external create_regular
    :  (t -> unit -> unit)
    -> Time.t
    -> Time.Span.t
    -> t
    = "lev_periodic_create_regular"

  external create_custom
    :  (t -> unit -> unit)
    -> (t -> now:Time.t -> Time.t)
    -> t
    = "lev_periodic_create_custom"

  let create f kind =
    let f = wrap_callback f in
    match kind with
    | Custom rb -> create_custom f rb
    | Regular { offset; interval } ->
      let interval =
        match interval with
        | None -> Time.Span.zero
        | Some f -> f
      in
      create_regular f offset interval
  ;;

  external stop : t -> Loop.t -> unit = "lev_periodic_stop"
  external start : t -> Loop.t -> unit = "lev_periodic_start"
end

module Timer = struct
  type t

  include Watcher (struct
      type nonrec t = t
    end)

  external create
    :  (t -> unit -> unit)
    -> Time.Span.t
    -> Time.Span.t
    -> t
    = "lev_timer_create"

  let create ?(repeat = Time.Span.zero) ~after f = create (wrap_callback f) after repeat

  external remaining : t -> Loop.t -> Time.Span.t = "lev_timer_remaining"
  external stop : t -> Loop.t -> unit = "lev_timer_stop"
  external start : t -> Loop.t -> unit = "lev_timer_start"
  external again : t -> Loop.t -> unit = "lev_timer_again"
end

module Signal = struct
  type t

  include Watcher (struct
      type nonrec t = t
    end)

  external stop : t -> Loop.t -> unit = "lev_signal_stop"
  external start : t -> Loop.t -> unit = "lev_signal_start"
  external create : (t -> unit -> unit) -> signal:int -> t = "lev_signal_create"

  let create f ~signal = create (wrap_callback f) ~signal
end

module Child = struct
  type t

  include Watcher (struct
      type nonrec t = t
    end)

  external stop : t -> Loop.t -> unit = "lev_child_stop"
  external start : t -> Loop.t -> unit = "lev_child_start"

  type pid =
    | Any
    | Pid of int

  type trace =
    | Terminate
    | Terminate_stop_or_continue

  external create
    :  (t -> pid:int -> Unix.process_status -> unit)
    -> int
    -> int
    -> t
    = "lev_child_create"

  let create cb pid trace =
    let pid =
      match pid with
      | Any -> 0
      | Pid pid -> pid
    in
    let trace =
      match trace with
      | Terminate -> 0
      | Terminate_stop_or_continue -> 1
    in
    create cb pid trace
  ;;

  let create = if Sys.win32 then Error `Unimplemented else Ok create
end

module Cleanup = struct
  type t

  include Watcher (struct
      type nonrec t = t
    end)

  external stop : t -> Loop.t -> unit = "lev_cleanup_stop"
  external start : t -> Loop.t -> unit = "lev_cleanup_start"
  external create : (t -> unit -> unit) -> t = "lev_cleanup_create"

  let create f = create (wrap_callback f)
end

module Stat = struct
  type t

  include Watcher (struct
      type nonrec t = t
    end)

  external destroy : t -> unit = "lev_stat_destroy"
  external stop : t -> Loop.t -> unit = "lev_stat_stop"
  external start : t -> Loop.t -> unit = "lev_stat_start"
  external create : (t -> unit -> unit) -> string -> Time.Span.t -> t = "lev_stat_create"
  external stat : t -> Unix.stats = "lev_stat_stat"

  let create_unix ?(interval = Time.Span.zero) ~path f =
    create (wrap_callback f) path interval
  ;;

  let create = if Sys.win32 then Error `Unimplemented else Ok create_unix
end

module Embed = struct
  type t

  include Watcher (struct
      type nonrec t = t
    end)

  external destroy : t -> unit = "lev_embed_destroy"
  external stop : t -> Loop.t -> unit = "lev_embed_stop"
  external start : t -> Loop.t -> unit = "lev_embed_start"

  type sweep =
    | Automatic
    | Manual of (t -> unit)

  external create_automatic : Loop.t -> t = "lev_embed_create_automatic"
  external create_manual : (t -> unit -> unit) -> Loop.t -> t = "lev_embed_create_manual"

  let create sweep loop =
    match sweep with
    | Automatic -> create_automatic loop
    | Manual f -> create_manual (wrap_callback f) loop
  ;;

  external sweep : t -> Loop.t -> unit = "lev_embed_sweep"
end

module Idle = struct
  type t

  include Watcher (struct
      type nonrec t = t
    end)

  external stop : t -> Loop.t -> unit = "lev_idle_stop"
  external start : t -> Loop.t -> unit = "lev_idle_start"
  external create : (t -> unit -> unit) -> t = "lev_idle_create"

  let create f = create (wrap_callback f)
end

module Check = struct
  type t

  include Watcher (struct
      type nonrec t = t
    end)

  external stop : t -> Loop.t -> unit = "lev_check_stop"
  external start : t -> Loop.t -> unit = "lev_check_start"
  external create : (t -> unit -> unit) -> t = "lev_check_create"

  let create f = create (wrap_callback f)
end

module Async = struct
  type t

  include Watcher (struct
      type nonrec t = t
    end)

  external stop : t -> Loop.t -> unit = "lev_async_stop"
  external start : t -> Loop.t -> unit = "lev_async_start"
  external pending : t -> bool = "lev_async_pending"
  external send : t -> Loop.t -> unit = "lev_async_send"
  external create : (t -> unit -> unit) -> t = "lev_async_create"

  let create f = create (wrap_callback f)
end

module Prepare = struct
  type t

  include Watcher (struct
      type nonrec t = t
    end)

  external stop : t -> Loop.t -> unit = "lev_prepare_stop"
  external start : t -> Loop.t -> unit = "lev_prepare_start"
  external create : (t -> unit -> unit) -> t = "lev_prepare_create"

  let create f = create (wrap_callback f)
end
