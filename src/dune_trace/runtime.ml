[%%if ocaml_version >= (5, 0, 0) && not_defined_permissive oxcaml]

include Runtime_events

[%%if ocaml_version >= (5, 4, 0)]

let current_timestamp () = Some (Timestamp.get_current ())

[%%else]

let current_timestamp () = None

[%%endif]
[%%else]

include struct
  [@@@ocaml.warning "-32-34"]

  type runtime_counter = unit
  type runtime_phase = unit
  type cursor = unit

  module Timestamp = struct
    type t = unit

    let to_int64 () = 0L
  end

  module Callbacks = struct
    type t = unit

    let create ?runtime_begin ?runtime_end ?runtime_counter () =
      ignore runtime_begin;
      ignore runtime_end;
      ignore runtime_counter
    ;;
  end

  let runtime_counter_name () = ""
  let runtime_phase_name () = ""
  let start () = ()
  let pause () = ()
  let resume () = ()
  let create_cursor _ = ()
  let read_poll () () _ = 0
  let current_timestamp () = None
end

[%%endif]
