calc_server
===========

Sample TCP calculator server for `monkey`.

Build
-----

    $ rebar3 compile

Run
---

    $ rebar3 shell --apps calc_server

After running the server, you can test it using telnet:

    $ telnet localhost 9090
    10
    20.2
    sum
    mul
