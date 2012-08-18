%
% Exercise 2.3 - Side Effects
%
% A. Write a function which prints out the integers between 1 and N. Example:
% print(5).
% 1
% 2
% 3
% 4
% 5
% ok
% Hint: Use io:format("Number:~p~n",[N]).
%
% B. Write a function which prints out the even integers between 1 and N. Example:
% even_print(5).
% 2
% 4
% ok
% Hint: Use guards
% 
%
% Chad Gibbons
% cgibbons@alertlogic.com
% August 10, 2012
%

-module(side_effects).
-export([print/1, even_print/1]).

print(N) ->
  print(1, N).
print(I, N) when I < N ->
  io:format("Number:~p~n", [I]),
  print(I+1, N);
print(I, N) when I == N ->
  io:format("Number:~p~n", [I]).

even_print(N) ->
  even_print(1, N).
even_print(I, N) when I < N ->
  case I rem 2 of
    0 -> io:format("Number:~p~n", [I]);
    _ -> false
  end,
  even_print(I+1, N);
even_print(I, N) when I == N ->
  case I rem 2 of
    0 -> io:format("Number:~p~n", [I]);
    _ -> ok
  end.
