%
% Exercise 2.1 - Evaluating Expressions
%
% A. Write a function sum/1 which given a positive integer N will return the 
% sum of all the integers between 1 and N.
% Example:sum(5) ⇒ 15.
%
% B. Write a function sum_interval/2 which given two integers N and M, where N 
% =< M, will return the sum of the interval between N and M. If N > M, you want 
% your process to terminate abnormally.
% Example:sum_interval(1,3) ⇒ 6.
% sum_interval(6,6) ⇒ 6.
%
% Chad Gibbons
% dcgibbons@gmail.com
% August 10, 2012
%

-module(sum).
-export([sum/1, sum_interval/2]).

sum(N) when N > 1 ->
  N + sum(N - 1);
sum(1) ->
  1.

sum_interval(N, M) when N < M ->
  N + sum_interval(N + 1, M);
sum_interval(N, M) when N == M ->
  N;
sum_interval(N, M) when N > M ->
  {error, "N > M"}.
