-module(area_server1).
-export([loop/0,rpc/2]).

loop() ->
    receive
        {From, {rectangle, Width, Ht}} ->
            From ! Width * Ht,
            loop();
        {From, {circle, R}} ->
            From ! 3.14159 * R * R,
            loop();
        Other ->
            io:format("I don't know what the rea of a ~p is ~n", [Other]),
            loop()
    end.

rpc(Pid, Request) ->
    Pid ! {self(), Request},
    receive
        Response ->
            Response
    end.
