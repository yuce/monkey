http_server
=====

Sample HTTP server for `monkey`.

Build
-----

    $ rebar3 compile

Run
---

    $ rebar3 shell --apps http_server

After running the server, you can test it using curl:

    $ curl -v http://localhost:9090/foo/bar