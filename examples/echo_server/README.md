echo_server
===========

Sample TCP echo server for `monkey`.

Build
-----

    $ rebar3 compile

Run
---

    $ rebar3 shell --apps echo_server

After running the server, you can test it using telnet:

    $ telnet localhost 9999
