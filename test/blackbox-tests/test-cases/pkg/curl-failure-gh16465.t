Test that Dune captures the error messages returned by curl upon failures to
fetch the sources for a package and displays them correctly to the user.
See https://github.com/ocaml/dune/issues/16465

  $ make_lockdir

  $ make_lockpkg foo <<EOF
  > (version 0.0.1)
  > (source
  >  (fetch
  >   (url "http://0.0.0.0:1")))
  > EOF

6 is CURLE_COULDNT_RESOLVE_HOST, the case reported in the issue.

  $ export FAKE_CURL_EXIT_CODE=6
  $ build_pkg foo
  File "dune.lock/foo.pkg", line 4, characters 7-25:
  4 |   (url "http://0.0.0.0:1")))
             ^^^^^^^^^^^^^^^^^^
  Error: 'curl' returned an invalid error code 6
         
         
  [1]



7 is CURLE_COULDNT_CONNECT.

  $ export FAKE_CURL_EXIT_CODE=7
  $ build_pkg foo
  File "dune.lock/foo.pkg", line 4, characters 7-25:
  4 |   (url "http://0.0.0.0:1")))
             ^^^^^^^^^^^^^^^^^^
  Error: 'curl' returned an invalid error code 7
         
         
  [1]



28 is CURLE_OPERATION_TIMEDOUT.

  $ export FAKE_CURL_EXIT_CODE=28
  $ build_pkg foo
  File "dune.lock/foo.pkg", line 4, characters 7-25:
  4 |   (url "http://0.0.0.0:1")))
             ^^^^^^^^^^^^^^^^^^
  Error: 'curl' returned an invalid error code 28
         
         
  [1]



35 is CURLE_SSL_CONNECT_ERROR.

  $ export FAKE_CURL_EXIT_CODE=35
  $ build_pkg foo
  File "dune.lock/foo.pkg", line 4, characters 7-25:
  4 |   (url "http://0.0.0.0:1")))
             ^^^^^^^^^^^^^^^^^^
  Error: 'curl' returned an invalid error code 35
         
         
  [1]



Less common codes are reported just as well, since curl explains itself for
all of them. 22 is CURLE_HTTP_RETURNED_ERROR.

  $ export FAKE_CURL_EXIT_CODE=22
  $ build_pkg foo
  File "dune.lock/foo.pkg", line 4, characters 7-25:
  4 |   (url "http://0.0.0.0:1")))
             ^^^^^^^^^^^^^^^^^^
  Error: 'curl' returned an invalid error code 22
         
         
  [1]



60 is CURLE_PEER_FAILED_VERIFICATION, a common one behind TLS-intercepting
proxies.

  $ export FAKE_CURL_EXIT_CODE=60
  $ build_pkg foo
  File "dune.lock/foo.pkg", line 4, characters 7-25:
  4 |   (url "http://0.0.0.0:1")))
             ^^^^^^^^^^^^^^^^^^
  Error: 'curl' returned an invalid error code 60
         
         
  [1]



63 is CURLE_FILESIZE_EXCEEDED.

  $ export FAKE_CURL_EXIT_CODE=63
  $ build_pkg foo
  File "dune.lock/foo.pkg", line 4, characters 7-25:
  4 |   (url "http://0.0.0.0:1")))
             ^^^^^^^^^^^^^^^^^^
  Error: 'curl' returned an invalid error code 63
         
         
  [1]



255 is an invalid exit code that we use for testing how our code handles them.

  $ export FAKE_CURL_EXIT_CODE=255
  $ build_pkg foo
  File "dune.lock/foo.pkg", line 4, characters 7-25:
  4 |   (url "http://0.0.0.0:1")))
             ^^^^^^^^^^^^^^^^^^
  Error: 'curl' returned an invalid error code 255
         
         
  [1]


