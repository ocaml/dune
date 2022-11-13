# Runtime RPC versioning implementation notes

This document describes the versioning protocol used to ensure two-way
compatibility between different versions of the API.

The approach is loosely inspired by the `Both_converts` model used by
[Versioned_rpc](https://ocaml.janestreet.com/ocaml-core/latest/doc/async_rpc_kernel/Async_rpc_kernel/index.html#module-Versioned_rpc)
in `Async`, in which both parties maintain a "menu" of supported RPC
versions, which is used to negotiate a common protocol for each
method.

This is a working document and will be updated as the design evolves.

## Terms

- A **procedure** is a common term encompassing *notifications*
    (one-way messages) and *requests* (a communication to which
    a response is expected).

- A **model** is the logical payload type for one direction of
    a procedure. Note that this is a per-actor entity; the ultimate
    goal of runtime RPC versioning is to allow clients and servers to
    disagree on a model type without preventing them from interacting.

- A **wire type** for a procedure is the logical type sent "over the
    wire" for one direction of a procedure. The end result of
    negotiating a version for a procedure is to select a wire type
    known to both the client and the server. Typically, the older of
    the two model types will be chosen as the wire type,

- A **generation** of a procedure is the set of wire types
    corresponding to each direction of a procedure, along with the
    de/serialisation logic and upgrade and downgrade functions
    transforming the wire types to the model types and vice versa.
    Each generation is associated with a *version number*, which
    should be unique within a procedure.

- The **menu** is a mapping from method names to the particular
    generations that will be used for each procedure for a particular
    session. In the source code, this term is overloaded to also refer
    to a mapping from method names to *all known* generations of
    a procedure.

- The **declaration** of a procedure lists its model types and all
    known generations, along with its method name. Multiple
    declarations of the same procedure is allowed, so long as they do
    not overlap version numbers.

- The **implementation** of a declaration is the actual behavior of
    a procedure, which acts on the model types. Typically, this will
    be on the server, but in the future there may also be a use for
    server-to-client requests. This document is not concerned with the
    internals of any given implementation, only whether such an
    implementation exists at all.

## Background

Previously, there was no distinction between model and wire types.
This meant that any change to a model type required both build servers
and clients to upgrade in lockstep, as otherwise the receiver would be
unable to deserialize the payload of a procedure.

Unfortunately, most lighter-weight solutions (such as modifying the
de/serialization logic to be resilient to, e.g., extra/missing fields
or variants in types) are insufficient. Early designs of the
diagnostic API, for example, reported targets as strings, but was
changed to give structured information instead.

Similarly, requirements like "the client must always be older than the
server" (or the reverse) don't work in environments like Jane Street,
where the same editor plugin must be able to interact seamlessly with
multiple iterations of Dune (which may be older or newer than the
editor plugin itself).

The main goal of the system, then, is to ensure that both the server
and client applications can be programmed against the current model
types for each procedure, with all backwards- or forwards- conversions
happening under the hood.

## Protocol

At session initialization time, the client will first send an
initialization request to the server containing a single version
number corresponding to the overall RPC version the client will use.
If this number is determined to be versioning-compatible (see
[Session versioning](#session-versioning)), the server will respond
with a token instructing the client to initiate version negotiation.
Otherwise, the server will respond with an error.

Upon receiving this token, the client will initiate version
negotiation by sending a list of `(method-name, generations)`
pairs, where `method-name` is the name of each declared procedure, and
`generations` is the list of version numbers for that procedure's
generations.

Upon receiving a list of supported versions from the client, the
server will compare it to its list of *implemented* versions,
selecting the greatest common generation for each procedure. If the
client and server do not share any generations for a procedure, it is
omitted entirely. If there is at least one method for which a common
version exists, then the server responds with a list of `(method-name,
selected-version)` pairs, where `selected-version` is the version
number of the greatest common generation. This list is then used by
both parties to construct the version menu. Otherwise, if there are no
common versions for any methods, an error is returned to the client
and the session is invalidated.

Note that we do not currently require declared/implemented versions to
span a contiguous range of version numbers. This can have a few uses,
such as preventing clients from using a known-bugged generation of
a procedure.

When executing a procedure, the sender first looks up the correct
generation in the menu (see [Error handling](#error-handling)), and
downgrades the payload from the sender-side model type to the wire
type. Upon receipt, the server performs the same lookup to deserialize,
then upgrade the payload to the receiver-side model type, then the
procedure implementation is performed, producing a response in the
case of requests. If necessary, the same transformations are then
performed in reverse, sending the value back to the sender, completing
the procedure.

Barring strange circumstances (such as a client declaring a generation
with a newer version number than the type exposed in `dune_rpc.mli`), it
is always the case that the transformation from wire to model types will
be the identity function on the side that is older.

## Miscellaneous implementation notes

### Session versioning

In addition to version numbers existing for each procedure version,
there are two further version numbers associated with the session as
a whole which are sent as part of session initialization.

The first is the version of Dune each side purports to be as
a `MAJOR.MINOR` number (serialized as an `int * int` pair). This is
not currently checked.

Next is a version of the initial handshake protocol to be used. This
takes the form of a single `int`. In the future, if the initial
negotiation protocol changes, this value can be adjusted and checked to
account for this.

### Error handling

Handling of versioning errors has become more complex, as we need to
distinguish between "no such method exists" and "the server and client
do not share any common generations for this method". Secondly, this
means that the initiation of a procedure can now fail, which
complicates one-way communications (for example, the server must
swallow errors and clients must be upgraded to handle version errors
on notifications, which were previously infallible).

Finally, the versioning protocol itself must be either versioned
separately or stabilised (see [Session versioning](#session-versioning)).

### Tweaks

- We currently send the entire version menu from client to server and
    back twice, once for the client to inform the server of all
    supported versions, and again for the server to inform the client
    of the common versions. This can lead to large messages being
    passed at session initialization, which may become a performance
    bottleneck.

    - The size of the version negotiation messages is proportional to
        the number of all known generations for all procedures, which
        can be approximated by `number-of-procedures` times
        `number-of-supported-generations`. In practice, I do not
        expect this number to be large (I would be surprised if this
        number is ever on the order of 100).

    - One alternative is to perform per-procedure negotiation, where
        the initiator of a procedure first sends its known version
        ranges, the recipient sends the selected version (or an
        error), then the procedure proceeds as before. This approach
        trades startup and lookup overhead for a constant
        per-communication overhead. It also makes distinguishing "no
        such method exists" and "no common versions" simpler.
