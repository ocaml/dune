module V1 = struct
  open Dune_rpc_private
  module Id = Id
  module Response = Response
  module Version_error = Version_error
  module Initialize = Initialize.Request
  module Call = Call
  module Loc = Loc
  module Target = Target
  module Diagnostic = Diagnostic
  module Path = Path
  module Progress = Progress
  module Message = Message
  module Where = Where
  module Registry = Registry
  include Public

  module Client = struct
    module type S = sig
      type t

      type 'a fiber

      type chan

      module Handler : sig
        type t

        val create :
             ?log:(Message.t -> unit fiber)
          -> ?abort:(Message.t -> unit fiber)
          -> unit
          -> t
      end

      module Versioned : sig
        type 'a notification

        type ('a, 'b) request

        val prepare_request :
             t
          -> ('a, 'b) Request.t
          -> (('a, 'b) request, Version_error.t) result fiber

        val prepare_notification :
             t
          -> 'a Notification.t
          -> ('a notification, Version_error.t) result fiber
      end

      val request :
           ?id:Id.t
        -> t
        -> ('a, 'b) Versioned.request
        -> 'a
        -> ('b, Response.Error.t) result fiber

      val notification : t -> 'a Versioned.notification -> 'a -> unit fiber

      val disconnected : t -> unit fiber

      module Stream : sig
        type 'a t

        val cancel : _ t -> unit fiber

        val next : 'a t -> 'a option fiber
      end

      val poll :
        ?id:Id.t -> t -> 'a Sub.t -> ('a Stream.t, Version_error.t) result fiber

      module Batch : sig
        type client := t

        type t

        val create : client -> t

        val request :
             ?id:Id.t
          -> t
          -> ('a, 'b) Versioned.request
          -> 'a
          -> ('b, Response.Error.t) result fiber

        val notification : t -> 'a Versioned.notification -> 'a -> unit

        val submit : t -> unit fiber
      end

      val connect :
           ?handler:Handler.t
        -> chan
        -> Initialize.t
        -> f:(t -> 'a fiber)
        -> 'a fiber
    end

    module Make = Client.Make
  end
end
