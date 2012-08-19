%
% Advanced Exercise 5 - Manipulating Lists
%
% A. Write a function which given a list of integers and an integer, will 
% return all integers smaller than or equal to that integer.
% Example: filter([1,2,3,4,5], 3) ⇒ [1,2,3].
%
% B. Write a function which given a lists will reverse the order of the 
% elements.  Example:reverse([1,2,3]) ⇒ [3,2,1].
%
% C. Write a function which, given a list of lists, will concatenate them.
% Example: concatenate([[1,2,3], [], [4, five]]) ⇒ [1,2,3,4,five]. Hint: You 
% will have to use a help function and concatenate the lists in several steps.
%
% D. Write a function which given a list of nested lists, will return a flat 
% list. Example: flatten([[1,[2,[3],[]]], [[[4]]], [5,6]]) ⇒ [1,2,3,4,5,6].
%
% Hint: use concatenate
%
% Chad Gibbons
% dcgibbons@gmail.com
% August 13, 2012
%

-module(mylists).
-export([filter/2, reverse/1, concatenate/1, flatten/1]).

filter([H|T], N) ->
  if
    H =< N ->
      [H|filter(T, N)];
    true ->
      filter(T, N)
  end;
filter([], _) ->
  [].

reverse(L) ->
  reverse(L, []).
reverse([H|T], NewList) ->
  reverse(T, [H|NewList]);
reverse([], NewList) ->
  NewList.

%
% concatenate([[1,2],[3,4]])
%   concatenate1([1,2], [[3,4]])
%     [1, ...]
%       concatenate1([2], [[3,4]])
%         [2, ...]
%           concatenate1([], [[3,4]])
%             concatenate([[3,4]])
%               concatenate1([3,4], [])
%                 [3, ...]
%                   concatenate1([4], [])
%                     [4, ...]
%                       concatenate1([], [])
%                         concatenate([])
%                           []
%                     [4]
%                [3, 4]
%         [2, 3, 4]
%     [1, 2, 3, 4]
%

concatenate([]) -> [];

% For normal concatenation, take the head element, which should be a list, and
% the tail, which should be a list of lists, and continue work.
concatenate([H|T]) -> concatenate1(H, T).

% Take the head of a the sublist element, and make a list of out of and the
% concatenation of the tail. We pass in the list of lists as an extra arg.
concatenate1([H|T], Lists) -> [H|concatenate1(T, Lists)];

% No more elements in the sublist, so we're all done with this list - continue
% on with the rest of the list of lists.
concatenate1([], Lists) -> concatenate(Lists).

flatten([H|T]) when is_list(H) ->
  concatenate([flatten(H), flatten(T)]);
flatten([H|T]) ->
  [H|flatten(T)];
flatten([]) ->
  [].

