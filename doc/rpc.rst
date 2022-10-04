.. _rpc:

********
Dune RPC
********

Starting from dune 3.0, dune's watch mode also runs an RPC server. This is a
general mechanism introduced to integrate with various dune functionality. Some
use cases we have in mind are:

- Text editors that would like to receive dune's error reporting
- In the future, custom actions that need to communicate with dune

There's no fixed scope for RPC, and we encourage users to submit requests to
cover more functionality.

The purpose of this documentation is to explain how RPC works, and how to
connect to it as a client. More concrete information such as what requests are
available is in Dune_rpc_

`dune-rpc` library
==================

We provide a client library ``dune-rpc`` to make it easy to write clients. The
library has a versioned interface and we guarantee to maintain its stability.
The library documentation describes the guarantees in more detail.

RPC clients written with this library are guaranteed to work for all versions of
the RPC server greater than it. In other words, a client using version ``X``
will work with all servers ``>= X``. However, it will not work with any servers
``< X``.

The library contains a client implementation parameterized over a concurrency
monad. It should be trivial to provide an implementation using the lwt_ library
for example.

The library provides an API to do the following:

- Initialize an RPC session over a channel
- Send RPC requests and notifications
- Handle notifications received from the server
- Definitions of available requests, notifications, and their associated types.

Connecting
==========

To connect to Dune's RPC server, it needs to be started in watch mode. It is
possible to use ``dune build --passive-watch-mode`` to start an RPC server which
will listen for requests without starting a build by itself. Then ``dune rpc
build .`` will connect to it, trigger a build, and report status.

.. _lwt: https://github.com/ocsigen/lwt
.. _Dune_rpc: https://github.com/ocaml/dune/blob/main/otherlibs/dune-rpc/dune_rpc.mli
