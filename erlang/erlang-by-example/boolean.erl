%
% Exercise 1.4: Simple Patttern Matching
%
% Write a module boolean.erl that takes logical expressions and boolean values 
% (represented as the atoms true and false) and returns their boolean result.  
% The functions you should write should include b_not/1, b_and/2 and b_or/2You 
% may not use the logical constructs and, or or not. Test your module from the 
% shell.
%
% Chad Gibbons
% dcgibbons@gmail.com
% August 10, 2012
%

-module(boolean).
-export([b_not/1, b_and/2, b_or/2]).

b_not(X) ->
  case X of
    true -> false;
    false -> true
  end.

b_and(X, Y) ->
  case X of
    true -> 
      case Y of
        true -> true;
        false -> false
      end;
    false -> 
      case Y of
        true -> false;
        false -> true
      end
  end.

b_or(X, Y) ->
  case X of
    true ->
      true;
    false ->
      case Y of
        true ->
          true;
        false ->
          false
      end
  end.
