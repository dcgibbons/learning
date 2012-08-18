%
% Exercise 2.3 - Creating Lists
%
% A. Write a function which returns a list of the format [1,2,..,N-1,N].
% Example:create(3) ⇒ [1,2,3].
%
% B. Write a function which returns a list of the format [N, N-1,..,2,1].
% Example: reverse_create(3) ⇒ [3,2,1].
%
% Chad Gibbons
% cgibbons@alertlogic.com
% August 10, 2012
%

-module(create).
-export([create/1, reverse_create/1]).

create(N) ->
  create(N, []).
create(N, L) when N > 1 ->
  create(N-1, [N|L]);
create(N, L) when N == 1 ->
  [1|L].

reverse_create(N) ->
  reverse_create(N, 1, []).
reverse_create(N, I, L) when I < N ->
  reverse_create(N, I+1, [I|L]);
reverse_create(N, I, L) when I == N ->
  [I|L].
