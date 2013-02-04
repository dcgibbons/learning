%
% Exercise 4-1: An Echo Server
%
% Write a server which will wait in a receive loop until a message is sent to 
% it. Depending on the message, it should either print it and loop again or 
% terminate. You want to hide the fact that you are dealing with a process, and 
% access its services through a functional interface. These functions will 
% spawn the process and send messages to it. The module echo.erl should export 
% the following functions. 
%
% Interface:
%  ￼echo:start() ⇒ ok.  
%   echo:stop() ⇒ ok. 
%   echo:print(Term) ⇒ ok.
%
% Chad Gibbons
% dcgibbons@gmail.com
% August 14, 2012
%

-module(echo).
-export([start/0, stop/0, print/1]).

start() ->
  register(echo, spawn(echo, loop, [])).

stop() ->
  ok.

loop() ->
  receive
    {From, Msg} ->
      From ! {self(), Msg},
      loop();
    stop -> 
      true
  end.

print(Term) ->
  ok.
