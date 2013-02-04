%
% hello.erl - Hello World for Erlang
%
% Chad Gibbons
% dcgibbons@gmail.com
% August 12, 2012
%

-module(hello).
-export([start/0]).

start() ->
  io:format("Hello, World!\n").

