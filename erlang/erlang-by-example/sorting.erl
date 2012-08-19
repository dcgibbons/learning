%
% Advanced Exercise 2.6 - Implement Quicksort and Merge sort
%
% Implement the following algorithms over lists:
%
% Quicksort
% The head of the list is taken as the pivot; the list is then split according 
% to those elements smaller than the pivot and the rest. These two lists are 
% then recursively sorted by quicksort and joined together with the pivot 
% between them.
%
% Merge sort
% The list is split into two lists of (almost) equal length. These are then 
% sorted separately and their result merged together.
%
% Chad Gibbons
% dcgibbons@gmail.com
% August 13, 2012
%

-module(sorting).
-export([quicksort/1, mergesort/1]).

quicksort([Pivot | RestOfList]) ->
  {Smaller, Larger} = partition(Pivot, RestOfList),
  quicksort(Smaller) ++ [Pivot] ++ quicksort(Larger).

partition(Pivot, List) ->
  partition(Pivot, List, {[], []}).

partition(_Pivot, [], Acc) ->
  Acc;
partition(Pivot, [Smaller|Rest], {S, L}) when Smaller =< Pivot ->
  partition(Pivot, Rest, {[Smaller|S], L});
partition(Pivot, [Larger|Rest], {S, L}) ->
  partition(Pivot, Rest, {S, [Larger|L]}).

mergesort([]) -> 
  [];
mergesort([X]) ->
  [X];
mergesort(L) when is_list(L) ->
  {Left, Right} = split(length(L) div 2, L),
  merge(mergesort(Left), mergesort(Right)).

% Splits a list at the point N and returns both parts.
split(N, List) ->
  split(N, List, []).
split(0, List, Acc)  ->
  {Acc, List};
split(N, [H|T], Acc) ->
  split(N-1, T, [H|Acc]).

merge([], Right) ->
  Right;
merge(Left, []) ->
  Left;
merge(Left = [L|Ls], Right=[R|Rs]) ->
  if L =< R ->
      [L | merge(Ls, Right)];
    L > R ->
      [R | merge(Left, Rs)]
  end.

