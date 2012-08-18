%
% hello.erl - Hello World for Erlang
%
% Chad Gibbons
% dcgibbons@gmail.com
% August 12, 2012
%

-module(hello).
-export([hello/0]).

hello() ->
  io:format("Hello, World!\n").

