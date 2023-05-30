# RPC Client Example

This project contains an executable `rpc_client` which connects to the RPC
server started when Dune is run in watch mode. To use this program, start Dune
in watch mode:

```
$ dune build --watch
```

Then run `rpc_client` from the same directory as `dune` was run from. The
`rpc_client` program will connect to the server (`dune build --watch` starts an
RPC server) and perform several RPC calls to it before instructing the server
(over RPC) to shutdown.
